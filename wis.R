library(epipredict)

wis <- function(x, actual,...) {
  UseMethod("wis")
}

wis.distribution <- function(x, actual, ...) {
  rlang::check_dots_empty()
  epipredict:::map2_dbl(vctrs::vec_data(x), actual, wis_dist_quantile)
}

wis_dist_quantile <- function(x, actual) {
  q <- vctrs::field(x, "q")
  if (all(is.na(q))) return(NA)
  if (is.na(actual)) return(NA)
  tau <- vctrs::field(x, "tau")
  2 * mean(pmax(
    tau * (actual - q),
    (1 - tau) * (q - actual), na.rm = TRUE
  ))
}
