# Auxiliary functions


# Function to calculate time series correlations
getCorrelation <- function(country, unit = "weekly", nonzero = F) {
  
  getExcessMortality(country, unit) -> E
  getVaccinated(country, unit) -> V
  getCovidDeaths(country, unit) -> C
  
  if (nonzero) {
    startvac <- which(V$new_vaccinations_smoothed_per_million > 0)[1]
    V[startvac:nrow(V), ] -> V
  }
  
  E %>%
    left_join(C, by = "date") %>%
    left_join(V, by = "date") %>%
    as_tibble() %>%
    select(!date) -> x
  
  R2 <- cor(x, use = "na.or.complete")
  
  return(c(R2[1, 2], R2[1, 3]))
}


# Function that finds the best lag between the time series
getBestLag <- function(country, unit = "weekly") {
  
  getExcessMortality(country, unit) -> E
  getVaccinated(country, unit) -> V
  getCovidDeaths(country, unit) -> C
  
  #startvac <- which(V$new_vaccinations_smoothed_per_million > 0)[1]
  #V[startvac:nrow(V), ] -> V
  
  E %>%
    left_join(C, by = "date") %>%
    left_join(V, by = "date") %>%
    as_tibble() %>%
    select(!date) -> x
  
  res1 <- ccf(x[1], x[2], na.action = na.omit, plot = F)
  res2 <- ccf(x[1], x[3], na.action = na.omit, plot = F)
  
  return(c(res1$lag[which.max(res1$acf)], res2$lag[which.max(res2$acf)]))
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
               left = richtext_grob(text = '<span style="color:darkgray">Excess deaths/1M</span> | <span style="color:red">COVID deaths/1M</span> | <span style="color:blue">Vaccinations/1k</span> per week',
                                    gp = gpar(cex = 1),
                                    rot = 90),
               padding = unit(1, "line"))
}


# Function to generate the symlog scale, taken from:
# https://stackoverflow.com/questions/14613355/how-to-get-something-like-matplotlibs-symlog-scale-in-ggplot-or-lattice
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

