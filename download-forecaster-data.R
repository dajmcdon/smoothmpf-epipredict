library(epidatr)
library(epiprocess)
library(dplyr)
library(tibble)

# grab data ---------------------------------------------------------------

fds <- readRDS("baseline-forecast-dates.rds")

hosp <- covidcast(
  "hhs",
  "confirmed_admissions_covid_1d",
  "day",
  "state",
  geo_values = "*",
  time_values = epirange(20200901, 20230101),
  issues = epirange(20200901, 20230101)) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, version = issue, hosp = value)

hosp <- hosp %>%
  left_join(epipredict::state_census %>% select(geo_value = abbr, pop)) %>%
  mutate(hosp = hosp / pop * 1e5, pop = NULL) %>%
  as_epi_archive(compactify = FALSE)

f <- function(x) {
  group_by(x, geo_value) %>%
    epi_slide(hosp7dav = mean(hosp), n = 7) %>%
    select(-hosp) %>%
    ungroup() %>%
    {tibble(results = list(.))}
}

hosp <- epix_slide(
  hosp,
  ~ f(.x),
  n = 365000,
  ref_time_values = fds, # only keep the fd as_ofs here (though cases has all)
  as_list_col = TRUE)

hosp <- hosp %>%
  select(-geo_value) %>%
  rename(version = time_value) %>%
  unnest(slide_value) %>%
  unnest(results) %>%
  as_epi_archive(compactify = FALSE)

cases <- covidcast(
  "jhu-csse",
  "confirmed_7dav_incidence_prop",
  "day",
  "state",
  time_values = epirange(20200901, 20230101),
  geo_values = "*",
  issues = epirange(20200901, 20230101)) %>%
  fetch_tbl()


cases <- cases %>%
  select(geo_value, time_value, version = issue, cases7dav = value) %>%
  as_epi_archive(compactify = FALSE)

ch_archive <- epix_merge(hosp, cases, sync = "locf", compactify = TRUE)

saveRDS(ch_archive$DT, "hosp-archive.rds")
