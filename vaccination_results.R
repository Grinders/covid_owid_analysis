library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(ggthemr)
library(arrow)


filename <- file.choose()
covid_data <- read_csv_arrow(filename)

#------------------------------------------------------------------------------
aggregated_data <- covid_data %>%
  select(
    location,
    date,
    new_deaths_per_million,
    new_cases_per_million,
    people_fully_vaccinated_per_hundred
  ) %>%
  group_by(location, date) %>%
  summarise(
    deaths = sum(new_deaths_per_million, na.rm = TRUE),
    cases = sum(new_cases_per_million, na.rm = TRUE),
    vaccinated = max(people_fully_vaccinated_per_hundred)
  ) %>%
  filter(
    (
      location == "Russia" || location == "Germany"
      || location == "France" || location == "United States"
      ||
        location == "United Kingdom" || location == "Poland"
      || location == "Romania" || location == "South Korea"
      || location == "Serbia" || location == "Czechia"
    )
  ) %>%
  filter( date >= (Sys.Date() %m-% months(12)))


ggthemr("flat")

hist_deaths_cmp <- aggregated_data %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_col(color = "darkslategray",
           alpha = 0.5) +
  geom_line(aes(x = date, y = vaccinated), color = 'blue') +
  facet_grid( . ~ location,
              scales = "free",
              space = "free",
              switch = "x") +
  ggtitle("Deaths per million per country") +
  labs(y = "Deaths per million", x = "Date", fill = "location") +
  scale_y_continuous("Deaths per million",
                     sec.axis = sec_axis( ~ ., name = "Vaccination rate"))


hist_deaths_cmp

hist_cases_cmp <- aggregated_data %>%
  ggplot(aes(x = date, y = cases)) +
  geom_col(color = "darkslategray",
           alpha = 0.5) +
  geom_line(aes(x = date, y = vaccinated), color = 'blue') +
  facet_grid( . ~ location,
              scales = "free",
              space = "free",
              switch = "x") +
  ggtitle("Cases per million per country") +
  labs(y = "Cases per million", x = "Date", fill = "location") +
  scale_y_continuous("Cases per million",
                     sec.axis = sec_axis( ~ ., name = "Vaccination rate"))


hist_cases_cmp
