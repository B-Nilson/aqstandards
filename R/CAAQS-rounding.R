## CAAQS metric arithmetic: the GDADs' percentile ranking approach,
## Percentile ranking approach from the CCME GDADs (e.g. NO2 GDAD, Appendix
## B): the p-th percentile of NDM available values is the Kth highest value
## in the decreasing ordered array, with K = NDM - Truncated(NDM * p) and
## "Truncated" discarding the decimal part (guarded by a small tolerance so
## that floating-point products like 100 * 0.99 = 98.999... truncate to the
## whole number the GDAD intends). Ties are repeated in rank order.
## Unlike stats::quantile() (type 7) no interpolation between order
## statistics is performed, and for NDM * p a whole number the value is the
## (NDM - NDM * p)th highest, not the NDM * p-th smallest. Returns NA for an
## empty input.
CAAQS_rank_percentile <- function(x, p) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) {
    return(NA_real_)
  }
  k <- n - floor(n * p + 1e-9)
  sort(x, decreasing = TRUE)[k]
}
## Decimal-place and rounding rules of the CCME Guidance Documents on
## Achievement Determination, applied before any comparison to a standard
## or management level. Each GDAD reports its metric values at a fixed
## number of decimal places (Ozone GDAD 2021 Table 5-4; NO2 and SO2 GDADs
## 2020 Table 5-4; PM2.5 GDAD 2012, PN 1483, Appendix D and sections
## 4.1.3, 4.2.2 and 4.2.3), and the Guidance Document on Air Zone
## Management (2019) requires that "the metric values for comparison to
## the concentrations must be rounded to the same number of digits as the
## shown concentrations" (Appendix 2, Tables A2-1 to A2-4). The GDADs'
## rounding is half-up: the O3/NO2/SO2 two-step procedure ("first discard
## all numbers after the first decimal ... if its decimal is 5, round
## upward; 4, round downward") and PM2.5's one-step convention ("numbers
## with first decimal .5 will be rounded upward; < .5 ... downward") both
## round upward exactly when the first discarded digit is 5 or more. The
## Ozone GDAD's worked example (Text Box 2: 3-year average 62.966... ppb
## becomes 62.9 ppb, which "is rounded upward 63 ppb, which is the
## calculated ozone CAAQS metric value") is reproduced by this helper.
## R's round() cannot be used because it rounds half to even (round(0.5)
## is 0 and round(0.15, 1) is 0.1).
CAAQS_round_gdad <- function(x, digits) {
  scale <- 10^digits
  ifelse(x >= 0, floor(x * scale + 0.5), -floor(-x * scale + 0.5)) / scale
}
## Reporting precision of each metric per the GDAD decimal-place tables
## (sources in CAAQS_round_gdad()): one decimal place for values "directly
## obtained from" reported concentrations (the daily maxima and annual
## percentiles of O3, NO2 and SO2; the PM2.5 daily 24-hr means and annual
## averages) and for the PM2.5 metric values; whole numbers for the O3
## metric value and the NO2/SO2 1-hour metric values (the 3-year averages
## of the percentile-type annual values).
CAAQS_metric_digits <- list(
  o3 = list(annual = 1L, `3yr` = 0L),
  no2 = list(annual = 1L, `3yr` = 0L),
  so2 = list(annual = 1L, `3yr` = 0L),
  pm25 = list(daily = 1L, annual = 1L, `3yr` = 1L)
)

## Annual-row exceptions criteria of the GDADs' data-completeness tables
## (Table 5-3 column 3; see CAAQS_completeness() for the quoted rows),
## applied to one metric's per-year values. A year meeting the metric's
## completeness criteria (`year_ok`) contributes its value; a gated-out
## year contributes its value only when the exception's conditions hold:
## the exception is configured for the pollutant, the relaxed first
## condition holds -- `relaxed_ok` is the "at least 50% of the [NO2 / SO2]
## 1-hour are available in each calendar quarter" criterion of the NO2/SO2
## annual metric value rows, and TRUE for the percentile-type rows whose
## exception has no such first condition -- and the value "exceeds the
## standard" (the CAAQS in force in the year, as in CAAQS_meets_standard()).
## Values of years meeting neither branch are NA. The metric values must
## be computed on all available data of each year beforehand: the annual
## exceptions are a validity decision, not a recomputation. All arguments
CAAQS_meets_standard <- function(year, metric, thresholds) {
  mgmt_levels <- thresholds[as.numeric(names(thresholds)) <= year] |>
    dplyr::last()
  if (length(mgmt_levels) == 0) {
    return(NA)
  }
  # Management levels per the Guidance Document on Air Zone Management
  # (2019) Appendix 2: Red is "greater than" the CAAQS (the achievement
  # determination GDADs' "less than or equal to" rule, so the Red edge is
  # exclusive), the Orange/Yellow lower edges are inclusive ("32 to 60
  # ppb" in the tables), and Green collects everything below the Yellow
  # lower edge ("< 50 ppb").
  red <- unname(mgmt_levels[["Red"]])
  band_edges <- mgmt_levels[c("Orange", "Yellow")]
  attainment <- rep(NA_character_, length(metric))
  attainment[!is.na(metric) & metric > red] <- "Red"
  for (lvl in names(band_edges)) {
    unclassified <- is.na(attainment)
    attainment[unclassified & metric >= band_edges[[lvl]]] <- lvl
  }
  attainment[is.na(attainment) & !is.na(metric)] <- "Green"
  return(attainment)
}
