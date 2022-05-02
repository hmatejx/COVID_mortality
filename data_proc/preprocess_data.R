# load required packages
require(tidyverse)
require(lubridate)
require(tsibble)
require(tempdisagg)
require(tsbox)
require(progress)
require(rprojroot)


root <- rprojroot::has_file(".git/index")

# load data
exmo <- suppressMessages(read_delim(root$find_file("data_raw", "excess-mortality-timeseries.csv"))) # excess mortality
c19 <- suppressMessages(read_delim(root$find_file("data_raw", "owid-covid-data.csv")))              # COVID-19 statistics (includes vacc.)
st <- suppressMessages(read_delim(root$find_file("data_raw", "covid-stringency-index.csv")))        # Country COVID-19 policy stringency
st <- as_tibble(st)

# select only relevant columns from the c19 data set and scale the country population millions
c19 %>% select(location, date,
               new_cases, new_cases_smoothed, new_cases_per_million, new_cases_smoothed_per_million,
               new_deaths, new_deaths_per_million, new_deaths_smoothed, new_deaths_smoothed_per_million,
               new_vaccinations, new_vaccinations_smoothed, new_vaccinations_smoothed_per_million,
               population) %>%
  mutate(pop_million = population / 1e6) -> c19


# fix some country names in the exmo data set so they match the name in the c19 data set
exmo %>%
  mutate(country_name = case_when(country_name == "Bosnia" ~ "Bosnia and Herzegovina",
                                  country_name == "Faroe Islands" ~ "Faeroe Islands",
                                  TRUE ~ country_name)) %>%
# and rename the excess deaths column
  rename(excess_deaths = `excess deaths`) -> exmo


# add country population to the exmo data set from the c19 data set
exmo %>%
  left_join(c19 %>% select(location, pop_million) %>% distinct(),
            by = c("country_name" = "location")) -> exmo


# set COVID-19 vaccination rate to 0 before first non-zero vaccination entry
# this takes several seconds, hence a progress bar...
pb <- progress_bar$new(total = length(unique(c19$location)))
pb$tick(0)
for (country in unique(c19$location)) {
  idx <- which(c19$location == country)
  suppressWarnings(first <- min(which(!is.na(c19$new_vaccinations_smoothed[idx]))))
  if (is.finite(first)) {
    torepl <- idx[1]:(idx[first - 1])
    c19$new_vaccinations[torepl] <- 0
    c19$new_vaccinations_smoothed[torepl] <- 0
    c19$new_vaccinations_smoothed_per_million[torepl] <- 0
  }
  pb$tick()
}


# Function to get the vaccination count, possible units = daily, weekly, monthly
getVaccinated <- function(country, unit = "weekly") {

  if (!(country %in% unique(c19$location))) {
    stop(paste0("Country `", country, "` not in list!"))
  }

  c19 %>%
    filter(location == country) %>%
    select(date, new_vaccinations_smoothed_per_million) %>%
    as_tsibble(index = date) -> x
    suppressWarnings(x %>%
      append_row(n = -as.integer(x$date[1] - as.Date("2020-01-01"))) -> x)
    x$new_vaccinations_smoothed_per_million[is.na(x$new_vaccinations_smoothed_per_million)] <- 0

  # We're done
  if (unit == "daily") {
    return(x)

  # Aggregate on weekly basis
  } else if (unit == "weekly") {
    return(
      x %>%
        index_by(week = ~ yearweek(.)) %>%
        summarise(new_vaccinations_smoothed_per_million = sum(new_vaccinations_smoothed_per_million)) %>%
        rename(date = week)) %>%
        mutate(date = date - 1) # reverse the aggregation shift

  # Aggregate on monthly basis
  } else if (unit == "monthly") {
    return(
      x %>%
        index_by(month = ~ yearmonth(.)) %>%
        summarise(new_vaccinations_smoothed_per_million = sum(new_vaccinations_smoothed_per_million)) %>%
        rename(date = month)) %>%
        mutate(date = date - 1) # reverse the aggregation shift
  }
}


# Function to get the vaccination count, possible units = weekly, monthly
getExcessMortality <- function(country, unit = "weekly") {

  if (!(country %in% unique(exmo$country_name))) {
    stop(paste0("Country `", country, "` not in list!"))
  }

  exmo %>%
    filter(country_name == country) %>%
    mutate(excess_deaths_per_million = excess_deaths / pop_million) -> x

  tu = x$time_unit[1]
  if (tu == "monthly") {
    x %>% mutate(date = yearmonth(paste0(year, "-", sprintf("%02d", time)))) -> x
  } else if (tu == "weekly") {
    x %>% mutate(date = yearweek(paste0(year, " W", sprintf("%02d", time)))) -> x
  } else if (tu == "quarterly") {
    x %>% mutate(date = yearquarter(paste0(year, " Q", time))) -> x
  }
  x %>%
    select(date, excess_deaths_per_million) %>%
    as_tsibble(index = date) -> x

  print(unit)
  print(tu)
  
  # We're done
  if (tu == unit) {
    return(x)
 
  # Disaggregate the time series in case the time unit is < than the data time unit
  } else if ((unit == "daily" && (tu == "weekly" || tu == "monthly" || tu == "quartrly")) ||
             (unit == "weekly" && (tu == "monthly" || tu == "quarterly")) ||
             (unit == "monthly" && tu == "quarterly")) {
    x <- ts_df(x)
    suppressMessages(m <- td(x ~ 1, to = unit, method = "denton-cholette"))
    return(
      predict(m) %>%
  #      mutate(date = time) %>%
        mutate(date = yearweek(time)) %>%
        select(date, value) %>%
        as_tsibble(index = date) %>%
        rename(excess_deaths_per_million = value)
    )

  # Aggregate the time series in case the time unit is > than the data time unit
  } else {
    return(
      x %>%
        index_by(month = ~ yearmonth(.)) %>%
        summarize(excess_deaths_per_million = sum(excess_deaths_per_million)) %>%
        rename(date = month) %>%
        mutate(date = date - 1) # reverse the aggregation shift
    )
  }
}


# Function to get the COVID death count, possible units = daily, weekly, monthly
getCovidDeaths <- function(country, unit = "weekly") {

  if (!(country %in% unique(c19$location))) {
    stop(paste0("Country `", country, "` not in list!"))
  }

  c19 %>%
    filter(location == country) %>%
    select(date, new_deaths_smoothed_per_million) %>%
    as_tsibble(index = date) -> x
  suppressWarnings(x %>%
                     append_row(n = -as.integer(x$date[1] - as.Date("2020-01-01"))) -> x)
  x$new_deaths_smoothed_per_million[is.na(x$new_deaths_smoothed_per_million)] <- 0

  # We're done
  if (unit == "daily") {
    return(x)

    # Aggregate on weekly basis
  } else if (unit == "weekly") {
    return(
      x %>%
        index_by(week = ~ yearweek(.)) %>%
        summarise(new_deaths_smoothed_per_million = sum(new_deaths_smoothed_per_million)) %>%
        rename(date = week) %>%
        mutate(date = date - 1) # reverse the aggregation shift
    )

    # Aggregate on monthly basis
  } else if (unit == "monthly") {
    return(
      x %>%
        index_by(month = ~ yearmonth(.)) %>%
        summarise(new_deaths_smoothed_per_million = sum(new_deaths_smoothed_per_million)) %>%
        rename(date = month) %>%
        mutate(date = date - 1) # reverse the aggregation shift
    )
  }
}



# save data
save(exmo, c19, st, getVaccinated, getExcessMortality, getCovidDeaths,
     file = root$find_file("data_proc", "input_data.RData"))
