library(epidatr)
library(dplyr)
source("wis.R")
fcasts <- readRDS("smooth-forecasts.rds")
fds <- readRDS("baseline-forecast-dates.rds")


# actual <- covidcast(
#   "hhs",
#   "confirmed_admissions_covid_1d",
#   "day",
#   "state",
#   geo_values = "*",
#   time_values = epirange(20210101, 20230201),
#   as_of = epirange(20230401, 20230401)) %>%
#   fetch_tbl() %>%
#   select(geo_value, time_value, hosp_truth = value)
# saveRDS(actual, "finalized-hospitalizations.rds")

actual <- readRDS("finalized-hospitalizations.rds")

score_fun <- function(fct) {
  tds <- range(fct$target_date)
  truth_data <- actual %>% filter(time_value >= tds[1], time_value <= tds[2])
  fct %>%
    select(geo_value, forecast_date, target_date, distn, ahead) %>%
    left_join(truth_data, by = c("geo_value", "target_date" = "time_value")) %>%
    mutate(wis = wis(distn, hosp_truth), distn = NULL, hosp_truth = NULL)
}

scores <- lapply(fcasts, score_fun)
scores <- bind_rows(scores)
saveRDS(scores, "scored-smooth-forecasts.rds")
