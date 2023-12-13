library(epidatr)
library(epiprocess)
library(epipredict)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(ggplot2)

source("smooth-forecaster.R")

ch_archive <- readRDS("hosp-archive.rds") %>%
  as_epi_archive()


# run the forecaster once -------------------------------------------------

fd <- as.Date("2021-01-01")

x <- epix_as_of(ch_archive, fd, min_time_value = fd - 60L)

p1 <- smoothfc(x)

p2 <- p1 %>%
  mutate(distn = nested_quantiles(distn)) %>%
  unnest(distn) %>%
  filter(tau %in% c(.1, .25, .5, .75, .9),
         geo_value %in% c("ny", "ca", "tx", "ar")) %>%
  pivot_wider(names_from = tau, values_from = q)


# plot data and forecasts -------------------------------------------------

z <- left_join(
  x,
  state_census %>% select(abbr, pop), by = c("geo_value" = "abbr")) %>%
  mutate(incident_hosp = hosp7dav * pop / 1e5) %>%
  filter(geo_value %in% c("ny", "ca", "tx", "ar"))
ggplot() +
  geom_ribbon(data = p2, aes(target_date, ymin = `0.1`, ymax = `0.9`),
              fill = "lightblue") +
  geom_ribbon(data = p2, aes(target_date, ymin = `0.25`, ymax = `0.75`),
              fill = "dodgerblue") +
  geom_line(data = p2, aes(target_date, `0.5`), color = "red") +
  facet_wrap(~ geo_value, scales = "free_y") +
  theme_bw() +
  geom_line(data = z, aes(time_value, incident_hosp), color = "darkgrey") +
  geom_vline(xintercept = ymd("2021-01-01"))


# rerun with no smoothing, takes a while ----------------------------------

wiggles <- smoothfc(x, 28L)

w2 <- wiggles %>%
  mutate(distn = nested_quantiles(distn)) %>%
  unnest(distn) %>%
  filter(tau %in% c(.1, .25, .5, .75, .9), geo_value %in% c("ny", "ca", "tx", "ar")) %>%
  pivot_wider(names_from = tau, values_from = q)


ggplot() +
  geom_ribbon(data = w2, aes(target_date, ymin = `0.1`, ymax = `0.9`),
              fill = "lightblue") +
  geom_ribbon(data = w2, aes(target_date, ymin = `0.25`, ymax = `0.75`),
              fill = "dodgerblue") +
  geom_line(data = w2, aes(target_date, `0.5`), color = "red") +
  facet_wrap(~ geo_value, scales = "free_y") +
  theme_bw() +
  geom_line(data = z, aes(time_value, incident_hosp), color = "darkgrey") +
  geom_vline(xintercept = ymd("2021-01-01"))

