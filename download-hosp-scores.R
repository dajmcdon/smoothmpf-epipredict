library(aws.s3)
library(dplyr)
library(tibble)

Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")
hosp_scores <- s3readRDS("score_cards_state_hospitalizations.rds", s3bucket)
hosp_scores <- hosp_scores %>%
  arrange(forecast_date, forecaster, geo_value, ahead)

min_forecast_date <- "2020-12-31"
max_forecast_date <- "2023-01-01"
fds <- hosp_scores %>%
  filter(forecaster == "COVIDhub-baseline",
         forecast_date > min_forecast_date,
         forecast_date < max_forecast_date) %>%
  pull(forecast_date) %>%
  unique()
saveRDS(fds, "baseline-forecast-dates.rds")


# EDA ---------------------------------------------------------------------

n_submitted <- hosp_scores %>%
  group_by(forecaster) %>%
  summarize(n = n(), min_fd = min(forecast_date)) %>%
  mutate(n = n / (50 * 28) ) %>% # roughly the number of submissions
  arrange(n)

baseline <- hosp_scores %>% filter(forecaster == "COVIDhub-baseline")
# baseline %>% group_by(forecast_date) %>%
#   summarise(n = n()) %>%
#   ggplot(aes(forecast_date, n)) +
#   geom_col() +
#   theme_bw()

# remove extra Hub forecasts
other_hub <- c("COVIDhub-trained_ensemble", "COVIDhub_CDC-ensemble",
               "COVIDhub-ensemble", "COVIDhub-baseline")
too_few_submissions <- n_submitted %>%
  filter(n < 20) %>% # require ~ 20 submissions
  pull(forecaster)

# filter to only 2021 and 2022 forecast dates, sufficient submissions
hosp_scores <- hosp_scores %>%
  filter(!(forecaster %in% other_hub),
         !(forecaster %in% too_few_submissions),
         forecast_date > min_forecast_date,
         forecast_date < max_forecast_date)


# Align forecast targets --------------------------------------------------
# (the alternative is to rerun the baseline manually on every dat in the fd set)
# teams could submit on any day for any ahead 1-28
# baseline and ensemble were only calculated weekly
# for each team, for each fd in the baseline set (fds),
#   1. use the most recent fd with fd <= fds (and within a week)
#   2. correct fd -> fds
#   3. recalculate ahead = td - fds
#   4. remove any aheads < 1
# The result is that every team now has the same set of fd as in the baseline
# set, AND a subset of the baseline target dates. Basically, an "early" forecast
# just loses out on information they could have had had they submitted a few
# days later


out <- list()
for (fd in seq_along(fds)) {
  ffd <- fds[fd]
  rec <- hosp_scores %>%
    filter(forecast_date <= ffd, forecast_date > ffd - 7)
  if (nrow(rec) > 0) {
    rec <- rec %>%
      group_by(forecaster, geo_value) %>%
      filter(forecast_date == max(forecast_date)) %>%
      ungroup()
    rec <- rec %>%
      mutate(forecast_date = fds[fd],
             ahead = as.numeric(target_end_date - forecast_date)) %>%
      filter(ahead > 0)
    out[[fd]] <- rec
  }
}
out <- bind_rows(out)
rm(rec)


# join the baseline -------------------------------------------------------

out <- out %>%
  select(forecaster, geo_value, forecast_date, ahead, wis)

saveRDS(out, "COVIDhub-scores.rds")
saveRDS(baseline %>% select(geo_value, forecast_date, ahead, wis),
        "basline-scores.rds")

smooth <- readRDS("scored-smooth-forecasts.rds") %>%
  select(-target_date) %>%
  mutate(forecaster = "Smooth-QMPF")

out <- left_join(
  out %>% bind_rows(smooth),
  baseline %>% select(geo_value, forecast_date, ahead, bwis = wis)
)

Mean <- function(x) mean(x, na.rm = TRUE)
GeoMean <- function(x, y = NULL, offset = 0) {
  x <- x + offset
  if (!is.null(y)) y <- y + offset
  else y <- 1
  exp(Mean(log(x / y)))
}

scores_summary <- out %>%
  group_by(forecaster, ahead) %>%
  summarise(relwis = GeoMean(wis, bwis, offset = 1))

saveRDS(scores_summary, "COVIDhub-hosp-scores-summary.rds")

# make a plot -------------------------------------------------------------
selected <- c("COVIDhub-4_week_ensemble", "CMU-TimeSeries", "Smooth-QMPF")

dl_models <- c(
  "GT-DeepCOVID",
  "CUB_PopCouncil-SLSTM",
  "Google_Harvard-CPF",
  "JHUAPL-Morris"
)
scores_summary <- scores_summary %>%
  mutate(
    anon = case_when(
      forecaster %in% selected ~ forecaster,
      forecaster %in% dl_models ~ "Deep learners",
      TRUE ~ "other"),
    anon = case_when(
      anon == "COVIDhub-4_week_ensemble" ~ "CDC Ensemble",
      TRUE ~ anon
    ))
cols <- c(
  "other" = "grey",
  "Deep learners" = "dodgerblue",
  "CDC Ensemble" = "red",
  "CMU-TimeSeries" = "purple",
  "Smooth-QMPF" = "orange"
)
library(ggplot2)
ggplot(scores_summary %>% filter(forecaster != "Smooth-QMPF"),
  aes(ahead, relwis, color = anon, group = forecaster)) +
  geom_line() +
  geom_line(data = scores_summary %>% filter(forecaster == "Smooth-QMPF"),
            linewidth = 1.5) +
  geom_hline(yintercept = 1, color = "black", linewidth = 1.5) +
  ylab("Geometric Mean of WIS\n relative to baseline") +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  xlab("Days ahead") +
  theme_bw() +
  scale_x_continuous(breaks = c(1, 7, 14, 21, 28)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = cols)

ggsave("forecaster-wis.pdf", width = 8, height = 4)
