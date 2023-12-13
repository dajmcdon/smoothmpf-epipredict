# needs epipredict from the smooth-quant-reg branch
library(epiprocess)
library(epipredict)
library(dplyr)
source("smooth-forecaster.R")

fds <- readRDS("baseline-forecast-dates.rds")
ch_archive <- readRDS("hosp-archive.rds") %>%
  as_epi_archive()


# takes about 8 hours in serial
fcasts <- list()
for (i in seq_along(fds)) {
  print(paste(fds[i], "-- forecast:", i, "/", length(fds)))
  dat <- epix_as_of(ch_archive, fds[i], fds[i] - 90L)
  fcasts[[i]] <- smoothfc(dat)
}

saveRDS(fcasts, "smooth-forecasts.rds")
