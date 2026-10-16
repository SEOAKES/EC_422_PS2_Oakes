library(tidyquant)
library(tidyverse)
library(knitr)

unemp_raw <- tq_get(
   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
   get  = "economic.data",
   from = "1976-01-01"
 )
 
 head(unemp_raw)
 
 dir.create("data", showWarnings = FALSE)
 
 write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

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

unemp_recent <- unemp |>
  filter(date > "2014-12-01")

unemp_summary <-  unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  ) 
  
unemp_recent_summary <-  unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  ) 

unemp_top_increases <- unemp |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  arrange(region, desc(unemployment_rate_change)) |>
  ungroup()

unemp_top_decreases <- unemp |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  arrange(region, desc(unemployment_rate_change)) |>
  ungroup()
  
unemp_no_covid <- unemp |>
    filter_out(c(date >= "2020-01-01" & date <= "2020-12-01"))

unemp_top_increases_no_covid <- unemp_no_covid |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  arrange(region, desc(unemployment_rate_change)) |>
  ungroup() 
  
unemp_top_decreases_no_covid <- unemp_no_covid |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  arrange(region, desc(unemployment_rate_change)) |>
  ungroup()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate by US Region, 1976-2026", 
       x = "Unemployment Rate", 
       y = "Date") +
      theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate by US Region, 1976-2026", 
       x = "Unemployment Rate", 
       y = "Date") +
  facet_wrap(~region) +
  theme_minimal()

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, 
                          fill = region))  +
  geom_col() +
  labs(title = "Mean Unemployment Rate by US Region, 1976-2026", 
       x = "Region", 
       y = "Mean Unemployment Rate") +
  theme_minimal()

ggplot(unemp_no_covid, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate by US Region, 1976-2026 (excluding 2020)", 
       x = "Unemployment Rate", 
       y = "Date") +
  theme_minimal()


