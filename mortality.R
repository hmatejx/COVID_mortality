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
require(rprojroot)
source("auxiliary.R")

root <- rprojroot::has_file(".git/index")

# load data
load(root$find_file("data_proc", "input_data.RData"))


# Generate an array of plots
# of countries sorted by population
exmo %>%
  select(country_name, pop_million) %>%
  distinct() %>%
  filter(!is.na(pop_million)) %>%
  arrange(desc(pop_million)) %>%
  mutate(R_1 = NA, R_2 = NA, C = NA, V = NA, C_sig = NA, V_sig = NA) -> countries_to_plot
# The code below should be re-factored, extremely clumsy as is
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
      plotPanel(plots)
      plots <- list()
      i <- 0
    }
  }
  Corr <- getCorrelation(country, nonzero = T)
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


# Plot correlations
countries_to_plot %>%
  ggplot() +
  aes(x = R_2, y = R_1, size = pop_million, col = country_name) +
  geom_point() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  xlim(-1, 1) + 
  xlab("Correlation of excess deaths and vaccination") +
  ylim(-1, 1) + 
  ylab("Correlation of excess deaths and COVID-19 deaths") +
  geom_abline(slope = 1, intercept = 0, col = "black", size = 1, linetype = "dashed") -> p.corr
print(p.corr)


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


# Plot excess mortality vs. stringency index
st %>%
#  filter(year(Day) == 2021) %>%
  group_by(Entity) %>%
  summarize(stringency = quantile(stringency_index, 0.05)) -> x

exmo %>%
  #filter(year == 2021) %>%
  group_by(country_name) %>%
  summarize(excess = sum(excess_deaths / pop_million),
            time_unit = first(time_unit),
            pop = first(pop_million)) %>%
  mutate(excess = if_else(time_unit == "monthly", excess, 
                          if_else(time_unit == "weekly", excess*(365/7/12), excess/3)))-> y
y %>% 
  left_join(x, by = c("country_name" = "Entity")) -> xy

xy %>% 
  ggplot(aes(x = stringency, y = excess, size = pop)) +
  geom_point() +
  geom_smooth(method = 'lm', mapping = aes(weight = pop)) +
  xlab("5th-percentile of COVID19 stringency index") + 
  ylab("Maximum monthly excess mortality per million")
