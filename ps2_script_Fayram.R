library(tidyverse)
unemp_raw <- read_rds("data/unemp_raw.rds")
head(unemp_raw)

#Renaming
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "NYUR" ~ "New York",
      symbol == "TXUR" ~ "Texas"
    )
  )

#Wrangling
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

#Recent Data

#Post 2015 Observations
unemp_recent <- unemp |>
  mutate(
    unemp_recent = date >= as.Date("2015-01-01"))
