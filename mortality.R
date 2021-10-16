# load required packages
require(tidyverse)
require(gridExtra)
require(grid)
require(gridtext)
require(tidyverse)
require(lubridate)
require(tsibble)
require(tempdisagg)
require(tsbox)
require(progress)
require(scales)
require(plotly)

# load data
load("data_proc/input_data.RData")


# Function to calculate time series correlations
# the shift parameter aligns the vaccination & COVID deaths time
getCorrelation <- function(country, unit = "weekly") {

  getExcessMortality(country, unit) -> E
  getVaccinated(country, unit) -> V
  getCovidDeaths(country, unit) -> C

  E %>%
    left_join(C, by = "date") %>%
    left_join(V, by = "date") %>%
    as_tibble() %>%
    select(!date) -> x

  R2 <- cor(x, use = "na.or.complete")

  return(c(R2[1, 2], R2[1, 3]))
}


# Function to calculate time series regression
# predicting excess mortality as a function of COVD-19 deaths and vaccinations
getRegression <- function(country, unit = "weekly") {

  getExcessMortality(country, unit) -> E
  getVaccinated(country, unit) -> V
  getCovidDeaths(country, unit) -> C

  E %>%
    left_join(C, by = "date") %>%
    left_join(V, by = "date") %>%
    as_tibble() %>%
    select(!date) -> x

  m <- lm(excess_deaths_per_million ~ new_deaths_smoothed_per_million +
                                      new_vaccinations_smoothed_per_million,
          data = x)

  res <- summary(m)$coefficients
  if (nrow(res) == 1) {
    return(c(C = NA, V = NA, C_sig = NA, V_sig = NA))
  }
  else if (nrow(res) == 2) {
    return(c(C = res[2, 1], V = res[2, 4], C_sig = NA, V_sig = NA))
  } else {
    res <- c(res[2:3, c(1,4)])
    names(res) <- c("C", "V", "C_sig", "V_sig")
    return(res)
  }
}


# Plot excess mortality, COVID 19 deaths, and vaccinations data for a country
plotCountry <- function(country) {

  if (!(country %in% unique(exmo$country_name) &
        country %in% unique(c19$location))) {
    cat(paste0("Error! Country '", country, "' not in the dataset.\n"))
    return(NULL)

  } else {

    E <- getExcessMortality(country, unit = "weekly")
    V <- getVaccinated(country, unit = "weekly")
    D <- getCovidDeaths(country, unit = "weekly")

    E %>%
      ggplot(aes(x = date, y = excess_deaths_per_million)) +
      geom_area(fill = "gray") +
      geom_line(size = 0.5, color = "black") +
      geom_line(data = D,
                aes(x = date, y = new_deaths_smoothed_per_million),
                size = 0.5, color = "red") +
      geom_line(data = V,
                aes(x = date, y = new_vaccinations_smoothed_per_million/1000),
                size = 0.5, color = "blue") +
      ggtitle(country) + theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 7),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            axis.line = element_line(colour = "black", size = 0.1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(6, 0, 0, 3)) -> p

    # annotate plot with correlations
    Corr <- getCorrelation(country)
    lab1 <- substitute(bold(R^2~"="~r2.), list(r2. = round(Corr[1]^2, 3)))
    ann1 <-
      annotation_custom(textGrob(label = lab1,
                                 just = "left",
                                 x = unit(0.03, "npc"), y = unit(0.92, "npc"),
                                 gp = gpar(cex = 0.5, col = "red")))
    lab2 <- substitute(bold(R^2~"="~r2.), list(r2. = round(Corr[2]^2, 3)))
    ann2 <-
      annotation_custom(textGrob(label = lab2,
                                 just = "left",
                                 x = unit(0.03, "npc"), y = unit(0.80, "npc"),
                                 gp = gpar(cex = 0.5, col = "blue")))

    return(p + ann1 + ann2)
  }
}


# Auxiliary function to plot a rectangular panel of countries
plotPanel <- function(plots) {
  grid.arrange(grobs = plots,
               bottom = textGrob("Date",
                                 gp = gpar(cex = 1)),
               left = richtext_grob(text = '<span style="color:red">Deaths per million</span> | <span style="color:blue">Vacc. per thousand</span>',
                                    gp = gpar(cex = 1),
                                    rot = 90),
               padding = unit(1, "line"))
}


# Generate an array of plots
# of countries sorted by population
exmo %>%
  select(country_name, pop_million) %>%
  distinct() %>%
  filter(!is.na(pop_million)) %>%
  arrange(desc(pop_million)) %>%
  mutate(R_1 = NA, R_2 = NA, C = NA, V = NA, C_sig = NA, V_sig = NA) -> countries_to_plot

# Warning... ugly code below
plots <- list()
i <- j <- 0
for (country in countries_to_plot$country_name) {
  j <- j + 1
  cat("Country = ", country, "\n")
  p <- plotCountry(country)
  if (!is.null(p)) {
    i <- i + 1
    plots[[i]] <- p
    if (i %% 25 == 0) {
      cat("Producing plot...\n")
      #plotPanel(plots)
      plots <- list()
      i <- 0
    }
  }
  Corr <- getCorrelation(country)
  countries_to_plot$R_1[j] <- Corr[1]
  countries_to_plot$R_2[j] <- Corr[2]

  Coef <- getRegression(country)
  countries_to_plot$C[j] <- Coef[1]
  countries_to_plot$V[j] <- Coef[2]
  countries_to_plot$C_sig[j] <- Coef[3]
  countries_to_plot$V_sig[j] <- Coef[4]
}
if (length(plots) > 0) {
  plotPanel(plots)
  plots <- list()
}


# plot correlations
countries_to_plot %>%
  ggplot() +
  aes(x = R_2^2, y = R_1^2, size = pop_million, col = country_name) +
  geom_point() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  xlim(0, 1) + xlab("Correlation of excess deaths and vaccination") +
  ylim(0, 1) + ylab("Correlation of excess deaths and COVID-19 deaths") +
  geom_abline(slope = 1, intercept = 0, col = "black", size = 1, linetype = "dashed") -> p.corr
print(p.corr)


symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))

  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             base^((sign(x) * x - thr) / scale) * thr)

  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(pretty_breaks()(c(min(abs(x)), thr)),
                       log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * log_breaks()(c(max(abs(x)), thr)),
                 pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-log_breaks(base)(c(thr, -x[1])),
                 pretty_breaks()(c(-thr, thr)),
                 log_breaks(base)(c(thr, x[2]))))
    }
  }
  trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}


# Plot regression coefficient for COVID-19 deaths
countries_to_plot %>%
  mutate(country_name = factor(country_name, levels = unique(country_name))) %>%
  ggplot(aes(x = country_name, y = C, fill = sapply(countries_to_plot$C_sig < 0.05, isTRUE))) +
  geom_bar(stat = "identity", color = "gray40", size = 0.1) +
  ylab("Regression coefficient for COVID-19 deaths") +
  scale_y_continuous(trans = symlog_trans(thr = 1, scale = 1), limits = c(-5, 50)) +
  scale_fill_manual("P < 0.05", values = c("TRUE" = "gray40", "FALSE" = "white"))+
  geom_hline(yintercept = 1, col = "black", size = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "top") -> pC.coef
print(pC.coef)


# Plot regression coefficient for Vaccinations
countries_to_plot %>%
  mutate(country_name = factor(country_name, levels = unique(country_name))) %>%
  ggplot(aes(x = country_name, y = V, fill = sapply(countries_to_plot$V_sig < 0.05, isTRUE))) +
  geom_bar(stat = "identity", color = "gray40", size = 0.1) +
  ylab("Regression coefficient for Vaccinations") +
  scale_y_continuous(trans = symlog_trans(thr = 0.0001, scale = 0.0001), limits = c(-2, 2)) +
  scale_fill_manual("P < 0.05", values = c("TRUE" = "gray40", "FALSE" = "white"))+
  geom_hline(yintercept = 1, col = "black", size = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "top") -> pV.coef
print(pV.coef)


st %>%
  filter(year(Day) == 2021) %>%
  group_by(Entity) %>%
  summarize(stringency = median(stringency_index)) -> x

exmo %>%
  filter(year == 2021) %>%
  group_by(country_name) %>%
  summarize(excess = sum(excess_deaths / pop_million),
            pop = mean(pop_million)) -> y
y %>% left_join(x, by = c("country_name" = "Entity")) -> xy

xy %>% ggplot(aes(x = stringency, y = excess, size = pop)) + geom_point() #+ ylim(c(0, 250))
