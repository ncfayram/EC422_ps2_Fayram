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

#Mean, max, min by region
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

head(unemp_summary)

#Slices
# Five largest month-over-month increases (plus ties)
unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

# Five largest month-over-month decreases (plus ties)
unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_no_covid <- unemp |>
  filter(lubridate::year(date) != 2020)
# Five largest month-over-month increases (plus ties)
unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

# Five largest month-over-month decreases (plus ties)
unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

