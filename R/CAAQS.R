#' Assess the attainment of the Canadian Ambient Air Quality Standards (CAAQS)
#'
#' @param dates Vector of hourly datetime values corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_1hr_ugm3 (Optional). Vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param o3_1hr_ppb (Optional). Vector of hourly mean ozone (O3) concentrations (ppb).
#' @param no2_1hr_ppb (Optional). Vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb).
#' @param so2_1hr_ppb (Optional). Vector of hourly mean sulphur dioxide (SO2) concentrations (ppb).
#'
#' @description
#' The Canadian Ambient Air Quality Standards (CAAQS) are part of a collaborative national Air Quality Management System (AQMS), to better protect human health and the environment.
#' Standards at various averaging periods are defined for fine particulate matter (PM2.5), ozone (O3), nitrogen dioxide (NO2), and sulphur dioxide (SO2), and are typically updated ever 5 years.
#'
#' Management levels (Green -> Yellow -> Orange -> Red) are defined for each pollutant standard.
#' A "Red" level indicates exceedance of the CAAQS and management plans are typically developed for regions at "Orange" or worse levels.
#'
#' Metrics follow the CCME Guidance Documents on Achievement Determination:
#' the O3 metric is the 3-year average of the annual 4th-highest daily maximum
#' 8-hour rolling average, with each rolling window assigned to the hour
#' ending the averaging period; the NO2 (SO2) hourly metric is the 3-year
#' average of the annual 98th (99th) percentile of daily maximum 1-hour
#' concentrations, with percentiles computed by the GDAD percentile ranking
#' approach (the Kth highest value, K = n - trunc(n * p), with no
#' interpolation); annual metrics are annual means of hourly concentrations; the
#' PM2.5 metrics are the 3-year average of the annual 98th percentile of daily
#' 24-hr means (over days with at least 18 available hours) computed by the
#' GDAD percentile ranking approach and the 3-year average of annual means of
#' valid daily values. For O3, NO2, SO2 and PM2.5 3-year metric values are
#' computed when at least two of the three annual values are available.
#'
#' Data completeness is assessed with the pollutant-specific criteria of the
#' guidance documents (Table 5-3 of the Ozone, NO2 and SO2 GDADs; sections
#' 4.1.4 and 4.2.4 of the PM2.5 GDAD; see `CAAQS_completeness()`): annual
#' metric values are reported only for years meeting the applicable daily,
#' annual and calendar-quarter criteria, and hours-per-year requirements are
#' derived from the calendar rather than hardcoded leap-year arithmetic. The
#' GDADs' exceptions to those criteria are applied: a deficient day or a
#' gated-out year is still retained when its value exceeds the standard (a
#' gated-out year's annual metric is computed from all available data), and
#' the NO2/SO2 annual-mean metric accepts the relaxed 50%-per-calendar-
#' quarter criterion of its Table 5-3 annual row when the annual average
#' exceeds the standard. Metric values are rounded per the GDADs'
#' decimal-place and rounding rules (Table 5-4 of the Ozone, NO2 and SO2
#' GDADs; Appendix D of the PM2.5 GDAD) before comparison to a standard or
#' management level, as the Guidance Document on Air Zone Management
#' (2019, Appendix 2) requires, and management levels are assigned with
#' that document's inclusive band edges (Red is strict `>`, the Orange and
#' Yellow lower edges inclusive, Green below the Yellow edge). Hourly
#' datetimes are assumed to label the start of the averaging hour and to
#' be in local standard time.
#'
#' @references
#' \itemize{
#'   \item CCME, Canadian Ambient Air Quality Standards (report page), \url{https://ccme.ca/en/air-quality-report}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Ozone (2021), \url{https://ccme.ca/en/res/gdadforozonecaaqsen.pdf}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Nitrogen Dioxide (2020), \url{https://ccme.ca/en/res/gdadforcaaqsfornitrogendioxide_en1.0.pdf}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Sulphur Dioxide (2020), \url{https://ccme.ca/en/res/gdadforcaaqsforsulphurdioxide_en1.0.pdf}
#'   \item CCME, Guidance Document on Achievement Determination: Canadian Ambient Air Quality Standards for Fine Particulate Matter and Ozone (2012, PN 1483), \url{https://ccme.ca/en/res/pn1483_gdad_eng-secured.pdf}
#'   \item CCME, Guidance Document on Air Zone Management (2019), \url{https://ccme.ca/en/res/guidancedocumentonairzonemanagement_secured.pdf}
#' }
#' @family Canadian Air Quality
#' @family Air Quality Standards
#'
#' @return a list of tibbles (data.frames), one tibble per pollutant provided with annual CAAQS metrics and management levels
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # Three years of hourly data: a constant background with a handful of
#' # elevated O3 plateau days. Every input is exact (no random generation),
#' # so the output is reproducible. data-raw/CAAQS-regression-fixtures.R
#' # holds a catalogue of rule-specific scenarios.
#' hours <- seq(
#'   lubridate::ymd_h("2021-01-01 00"),
#'   lubridate::ymd_h("2023-12-31 23"), "1 hours"
#' )
#' obs <- data.frame(
#'   date = hours,
#'   pm25 = rep(10, length(hours)),
#'   o3 = rep(30, length(hours)),
#'   no2 = rep(10, length(hours)),
#'   so2 = rep(1, length(hours))
#' )
#' for (day in paste0(rep(2021:2023, each = 4),
#'                    c("-06-10", "-06-20", "-07-10", "-07-20"))) {
#'   obs$o3[obs$date %in% seq(
#'     lubridate::ymd_h(paste(day, "09")),
#'     lubridate::ymd_h(paste(day, "16")), "1 hours"
#'   )] <- 70
#' }
#' CAAQS(
#'   dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#'   o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2, so2_1hr_ppb = obs$so2
#' )
CAAQS <- function(
  dates,
  pm25_1hr_ugm3 = NULL,
  o3_1hr_ppb = NULL,
  no2_1hr_ppb = NULL,
  so2_1hr_ppb = NULL
) {
  # Join inputs
  obs <- dplyr::bind_cols(
    date = dates,
    pm25 = pm25_1hr_ugm3,
    o3 = o3_1hr_ppb,
    no2 = no2_1hr_ppb,
    so2 = so2_1hr_ppb
  ) |>
    dplyr::mutate(year = lubridate::year(.data$date))

  # Assess data completeness for each pollutant annually, using the
  # pollutant-specific criteria of the CCME GDADs (see CAAQS_completeness()).
  has_enough_obs <- CAAQS_has_enough_obs(obs, CAAQS_completeness())

  # Check for 3 consecutive years for any pollutant. `lag()` returns NA for
  # the absent years created by tidyr::complete(), and NA + TRUE is NA, so
  # propagate a FALSE for the missing years before summing.
  has_3_consecutive_years <- has_enough_obs |>
    dplyr::summarise(dplyr::across(-"year", \(x) {
      x <- x |>
        handyr::swap(NA, with = FALSE) |>
        dplyr::coalesce(FALSE)
      any((x + dplyr::lag(x) + dplyr::lag(x, 2)) >= 3, na.rm = TRUE)
    }))
  if (all(!has_3_consecutive_years)) {
    stop(paste(
      "Cannot calculate CAAQS without at least one pollutant with at least 3 years",
      "of complete data."
    ))
  }

  # Keep data for years lacking enough data: whether a gated-out year's
  # data contribute a metric is decided inside CAAQS_<pollutant>() so that
  # the GDADs' annual exceptions criteria ("The [annual fourth highest /
  # 98th / 99th percentile based on the available data] exceeds the
  # standard") can retain such a year. Warn for transparency:
  pols <- names(has_enough_obs)[-1]
  for (pol in pols) {
    insufficient_years <- has_enough_obs$year[unlist(!has_enough_obs[pol])]
    if (length(insufficient_years)) {
      warning(paste(
        "Insufficient data collected for pol:",
        pol,
        "for year(s):",
        paste(insufficient_years, collapse = ", "),
        "see the CCME guidance documents' data completeness criteria"
      ))
    }
  }

  # Fill in missing hours with NAs
  obs <- obs |>
    tidyr::complete(
      date = seq(
        min(dates) |> lubridate::floor_date("years"),
        (max(dates) |> lubridate::ceiling_date("years")) - lubridate::hours(1),
        "1 hours"
      )
    ) |>
    dplyr::arrange(.data$date)

  # Calculate CAAQS attainment where data provided
  thresholds <- CAAQS_thresholds()
  list(
    pm25 = if (!is.null(pm25_1hr_ugm3)) CAAQS_pm25(obs, thresholds),
    o3 = if (!is.null(o3_1hr_ppb)) CAAQS_o3(obs, thresholds),
    no2 = if (!is.null(no2_1hr_ppb)) CAAQS_no2(obs, thresholds),
    so2 = if (!is.null(so2_1hr_ppb)) CAAQS_so2(obs, thresholds)
  )
}
