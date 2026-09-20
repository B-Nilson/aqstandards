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
#' obs <- data.frame(
#'   date = seq(
#'     lubridate::ymd_h("2020-01-01 00"),
#'     lubridate::ymd_h("2023-12-31 23"), "1 hours"
#'   ),
#'   pm25 = sample(1:150, 35064, TRUE), o3 = sample(1:150, 35064, TRUE),
#'   no2 = sample(1:150, 35064, TRUE), so2 = sample(1:150, 35064, TRUE)
#' )
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

## CAAQS Completeness -----------------------------------------------------
## Data-completeness configuration for each pollutant, one row per criterion,
## quoting the Data completeness criteria (column 2) of Table 5-3 of the CCME
## Guidance Document on Achievement Determination for each pollutant (Ozone
## 2021; Nitrogen Dioxide 2020; Sulphur Dioxide 2020; Fine Particulate Matter
## and Ozone 2012, PN 1483, sections 4.1.4 and 4.2.4 for PM2.5).
##
## Column 3 of the same tables lists the exceptions to those criteria; this
## configuration is also the single owner of which of those exceptions apply
## per pollutant, alongside the criteria they qualify:
##
##   * Daily-row exception ("The [O3 Dmax 8-hour / NO2 Dmax 1-hour / SO2 Dmax
##     1-hour] exceeds the standard"): a deficient day is retained when its
##     value exceeds the standard. Implemented in CAAQS_o3/no2/so2.
##
##   * Annual percentile-row exception (O3: "The annual fourth highest
##     exceeds the standard"; NO2: "The 98th percentile based on the
##     available NO2 Dmax 1-hour exceeds the standard"; SO2: "The 99th
##     percentile based on the available SO2 Dmax 1-hour exceeds the
##     standard"): a year failing the annual days/quarters criteria still
##     contributes its percentile-type metric when that metric, computed on
##     all available data of the year, exceeds the standard. Implemented in
##     the pipelines via `annual_metric_exception`.
##
##   * NO2/SO2 annual-metric-value exception ("1. at least 50% of the [NO2 /
##     SO2] 1-hour are available in each calendar quarter; and 2. the annual
##     average exceeds the standard"): the annual-mean metric of a year
##     failing the 75%-of-hours / 60%-per-quarter criteria is still computed
##     under the relaxed 50%-per-calendar-quarter criterion when the annual
##     average exceeds the standard. Implemented in the pipelines via
##     `annual_mean_exception`.
##
##   * PM2.5's 2012 GDAD predates the Table 5-3 format and has no
##     data-completeness exceptions column at all (its "exceptional events"
##     procedures in section 7 are an administrative TF/EE designation
##     process, not a data-completeness rule); none are implemented for
##     PM2.5.
CAAQS_completeness <- function() {
  list(
    # Ozone GDAD (2021), Table 5-3 (Ozone Dmax 8-hour): "O3 Dmax 8-hour are
    # available for at least 75% of the days in the period April 1 to
    # September 30." The annual fourth-highest ranking is restricted to that
    # season because it is "the time of year where the annual fourth highest
    # will likely be recorded in most of Canada" (GDAD section 5.3). The
    # annual row's column-3 exception ("The annual fourth highest exceeds
    # the standard") retains a year that fails the season days criterion
    # when its annual fourth highest, computed on all available data of the
    # year, exceeds the standard (annual_metric_exception below).
    o3 = list(
      min_hours_of_day = 18L,
      season = c(start = "04-01", end = "09-30"),
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = NULL,
      min_hours_fraction_year = NULL,
      min_hours_fraction_quarters = NULL,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE
    ),
    # Nitrogen Dioxide GDAD (2020), Table 5-3:
    #   NO2 Dmax 1-hour: "At least 18 of the 24 (75%) NO2 1-hour are
    #     available in the day."
    #   Annual 98th percentile of the NO2 Dmax 1-hour: "The NO2 Dmax 1-hour
    #     are available for at least: 1. 75% of the days in a year; and
    #     2. 60% of the days in each calendar quarter."
    #   Annual metric value: "1. at least 75% of the NO2 1-hour are available
    #     in the year; and 2. at least 60% of the NO2 1-hour are available in
    #     each calendar quarter."
    # Exceptions: the percentile row's exception ("The 98th percentile based
    # on the available NO2 Dmax 1-hour exceeds the standard") retains a year
    # failing the days/quarters criteria when its 98th percentile, computed
    # on all available data of the year, exceeds the standard
    # (annual_metric_exception below). The annual metric value row's
    # exception ("1. at least 50% of the NO2 1-hour are available in each
    # calendar quarter; and 2. the annual average exceeds the standard")
    # computes the annual-mean metric under the relaxed 50%-per-quarter
    # criterion when the annual average exceeds the standard
    # (annual_mean_exception below).
    # Calendar quarters (GDAD footnote): Q1 January 1 - March 31; Q2 April 1
    # - June 30; Q3 July 1 - September 30; Q4 October 1 - December 31.
    no2 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = 0.75,
      min_hours_fraction_quarters = 0.6,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE,
      annual_mean_exception = TRUE
    ),
    # Sulphur Dioxide GDAD (2020), Table 5-3: identical structure to NO2 with
    # SO2 in place of NO2 (SO2 Dmax 1-hour "At least 18 of the 24 (75%) ...
    # available in the day"; annual 99th percentile "75% of the days in a
    # year and 60% of the days in each calendar quarter"; annual metric
    # value "75% of the SO2 1-hour ... in the year and 60% ... in each
    # calendar quarter"). Exceptions: as for NO2, the daily-row exception
    # ("The SO2 Dmax 1-hour exceeds the standard") is implemented in
    # CAAQS_so2(), the percentile-row exception ("The 99th percentile based
    # on the available SO2 Dmax 1-hour exceeds the standard") via
    # annual_metric_exception, and the annual metric value row's exception
    # ("at least 50% ... in each calendar quarter; and 2. the annual average
    # exceeds the standard") via annual_mean_exception.
    so2 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = 0.75,
      min_hours_fraction_quarters = 0.6,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE,
      annual_mean_exception = TRUE
    ),
    # PM2.5 Guidance Document on Achievement Determination (PN 1483, "Fine
    # Particulate Matter and Ozone", 2012), sections 4.1.1, 4.1.4 and 4.2.4:
    # the daily 24hr-PM2.5 is valid when "at least 75% (18 hours) of the
    # 1-hour concentrations are available on the given day", and both the
    # annual 98P and the annual average require "at least 75% valid daily-
    # 24hr-PM2.5 in the year" and "at least 60% valid daily-24hr-PM2.5 in
    # each calendar quarter" (quarters Q1 January 1 - March 31 through Q4
    # October 1 - December 31). Unlike NO2/SO2 there is no hours-per-year
    # criterion: the annual gates are expressed in valid days only, and the
    # 2012 document has no exceptions column (see the header comment above).
    pm25 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = NULL,
      min_hours_fraction_quarters = NULL,
      min_days_of_quarters = NULL,
      annual_metric_exception = FALSE,
      annual_mean_exception = FALSE
    )
  )
}

## Calendar periods touched by `obs` (columns `date` and `year`), one row
## per calendar year: `days_in_year`, `hours_in_year`, `quarter` (Q1 to Q4,
## the GDADs' calendar quarters, Q1 January 1 - March 31 through Q4 October
## 1 - December 31), `days_in_quarter` and `hours_in_quarter`. All counts
## are derived from the calendar via lubridate's leap-year rule (which
## handles century years correctly), replacing the previously hardcoded
## `year %% 4 == 0` leap check.
CAAQS_cal_year_periods <- function(obs) {
  yrs <- sort(unique(obs$year))
  data.frame(year = rep(yrs, each = 4), quarter = paste0("Q", 1:4)) |>
    dplyr::mutate(
      days_in_year = 365 +
        as.integer(lubridate::leap_year(.data$year)),
      hours_in_year = 24 * .data$days_in_year,
      days_in_quarter = c(90, 91, 92, 92) +
        as.integer(.data$quarter == "Q1" & lubridate::leap_year(.data$year)),
      hours_in_quarter = 24 * .data$days_in_quarter
    )
}

## Per-year completeness details for the single pollutant configured by
## `cfg` (one entry of CAAQS_completeness(); see there for the quoted GDAD
## criteria and exceptions). Returns a one-row tibble:
##
##   * `days_ok` — the year's days criteria for the percentile-type metrics
##     hold: the year (or the configured season, for O3) holds at least
##     `min_days_fraction` of its calendar days with an available daily
##     value, and each calendar quarter at least
##     `min_days_fraction_quarters` of its days.
##   * `hours_ok` — the year's hourly criteria for the annual-mean metrics
##     hold: at least `min_hours_fraction_year` of the year's hours and
##     `min_hours_fraction_quarters` of each quarter's hours are available.
##   * `relaxed_quarters_ok` — every calendar quarter holds the GDADs'
##     relaxed 50%-of-hours criterion (the NO2/SO2 annual metric value
##     row's exception, "at least 50% ... in each calendar quarter"); the
##     complement of `hours_ok` for the quarters.
##
## A day has an available daily value when it meets the daily criterion of
## Table 5-3 (e.g. "At least 18 of the 24 (75%) NO2 1-hour are available in
## the day"); with no daily criterion any observed hour makes the day
## available. Deficient days are excluded from the metric pipelines (see
## CAAQS_o3/no2/so2) but do not by themselves make the year incomplete: the
## annual criteria are expressed in available days.
CAAQS_gate_details <- function(day_hours, cfg, cal) {
  valid_day <- if (!is.null(cfg$min_hours_of_day)) {
    day_hours$hours >= cfg$min_hours_of_day
  } else {
    day_hours$hours > 0
  }
  # Scope of the days gate: the season when one is configured (Ozone GDAD
  # Table 5-3: "O3 Dmax 8-hour are available for at least 75% of the days
  # in the period April 1 to September 30"), otherwise the whole year
  # (NO2/SO2 GDADs Table 5-3: "available for at least 75% of the days in a
  # year").
  scope <- valid_day
  if (!is.null(cfg$season)) {
    in_season <-
      day_hours$monthday >= cfg$season["start"] &
        day_hours$monthday <= cfg$season["end"]
    scope <- scope & in_season
  }

  # The GDAD denominators are the calendar days of the year (or of the
  # fixed April 1 - September 30 season, 183 days whether or not the year
  # is a leap year). cal has one row per calendar quarter, so its year
  # columns repeat the year's value 4 times.
  days_in_scope <- if (!is.null(cfg$season)) 183 else cal$days_in_year[1]
  days_ok <- TRUE
  if (!is.null(cfg$min_days_fraction)) {
    days_ok <- days_ok && sum(scope) >= days_in_scope * cfg$min_days_fraction
  }
  if (!is.null(cfg$min_days_of_year)) {
    days_ok <- days_ok && sum(valid_day) >= cfg$min_days_of_year
  }

  q <- cal |>
    dplyr::left_join(
      day_hours |>
        dplyr::mutate(
          quarter = paste0("Q", lubridate::quarter(.data$date, fiscal_start = 1)),
          valid = valid_day
        ) |>
        dplyr::group_by(.data$quarter) |>
        dplyr::summarise(
          .groups = "drop",
          days = sum(.data$valid),
          hours_avail = sum(.data$hours)
        ),
      by = "quarter"
    )
  if (!is.null(cfg$min_days_fraction_quarters)) {
    days_ok <- days_ok &&
      all(q$days >= q$days_in_quarter * cfg$min_days_fraction_quarters)
  }
  if (!is.null(cfg$min_days_of_quarters)) {
    days_ok <- days_ok && all(q$days >= cfg$min_days_of_quarters)
  }

  hours_ok <- TRUE
  if (!is.null(cfg$min_hours_fraction_year)) {
    # Annual hourly gate, e.g. "at least 75% of the NO2 1-hour are
    # available in the year" (NO2 and SO2 GDADs Table 5-3, annual metric
    # value criteria).
    hours_ok <- hours_ok &&
      sum(day_hours$hours) >= cfg$min_hours_fraction_year * cal$hours_in_year[1]
  }
  if (!is.null(cfg$min_hours_fraction_quarters)) {
    # Calendar-quarter hourly gate: "60% of the NO2 1-hour are available in
    # each calendar quarter" (NO2 and SO2 GDADs Table 5-3, annual metric
    # value criteria).
    hours_ok <- hours_ok &&
      all(q$hours_avail >= q$hours_in_quarter * cfg$min_hours_fraction_quarters)
  }

  tibble::tibble(
    days_ok = isTRUE(days_ok),
    hours_ok = isTRUE(hours_ok),
    relaxed_quarters_ok = isTRUE(all(q$hours_avail >= q$hours_in_quarter * 0.5))
  )
}

## Evaluate the year-level completeness criteria of `cfg` (one
## CAAQS_completeness() entry) for the hourly observations of a single
## pollutant (`pol`, a column of `obs_grouped`, grouped by year). Returns
## the CAAQS_gate_details() tibble per year (days_ok, hours_ok and
## relaxed_quarters_ok), the per-gate inputs the metric pipelines need for
## the GDADs' annual-row exceptions criteria.
CAAQS_year_gate_details <- function(obs_grouped, cfg, pol) {
  obs_grouped |>
    dplyr::group_modify(function(.x, .y) {
      # Extract the pollutant column with base R before any data masking:
      # .data[[pol]] pronoun subsetting is unreliable inside group_modify()'
      # s nested masks (it can silently read as all-NA for some groups).
      cal <- .x |>
        dplyr::distinct(
          date = .data$date |> lubridate::floor_date("days"),
          year = lubridate::year(.data$date)
        ) |>
        CAAQS_cal_year_periods()
      day_hours <- tibble::tibble(
        date = lubridate::floor_date(.x$date, "days"),
        hours = as.integer(!is.na(.x[[pol]]))
      ) |>
        dplyr::group_by(.data$date) |>
        dplyr::summarise(hours = sum(.data$hours), .groups = "drop") |>
        dplyr::mutate(
          monthday = sprintf(
            "%02d-%02d", lubridate::month(.data$date), lubridate::day(.data$date)
          )
        )
      CAAQS_gate_details(day_hours, cfg, cal)
    })
}

## Assess annual data completeness for each pollutant in `obs` (columns
## `date` plus one numeric column per pollutant) against the per-pollutant
## criteria in `completeness` (see CAAQS_completeness()). Returns a tibble
## with one row per calendar year in the data and one logical column per
## pollutant: TRUE only when every applicable criterion holds for the year.
## Pollutants absent from the configuration (no criterion at all) are never
## considered complete, so that unreviewed standards fail safe.
CAAQS_has_enough_obs <- function(obs, completeness) {
  if (is.null(obs$year)) {
    obs$year <- lubridate::year(obs$date)
  }
  pols <- names(completeness)
  pols <- pols[pols %in% names(obs)]
  obs <- obs |>
    dplyr::mutate(date = .data$date |> lubridate::floor_date("days"))
  cal <- CAAQS_cal_year_periods(obs)
  # One row per year x pollutant x day: distinct hours with a non-NA
  # observation, used by both the daily and the annual gates.
  avail <- obs |>
    tidyr::pivot_longer(dplyr::all_of(pols), names_to = "pol", values_to = "conc") |>
    dplyr::group_by(.data$year, .data$pol, .data$date) |>
    dplyr::summarise(hours = sum(!is.na(.data$conc)), .groups = "drop") |>
    dplyr::mutate(
      monthday = paste0(
        sprintf("%02d", lubridate::month(.data$date)),
        "-", sprintf("%02d", lubridate::day(.data$date))
      )
    )
  avail |>
    dplyr::group_by(.data$year, .data$pol) |>
    dplyr::group_modify(~ {
      cfg <- completeness[[.y$pol]]
      if (is.null(cfg)) {
        return(tibble::tibble(ok = FALSE))
      }
      cal <- cal[cal$year == .y$year, ]
      details <- CAAQS_gate_details(.x, cfg, cal)
      # TRUE only when every applicable criterion holds; the per-gate
      # details are what the pipelines use for the GDADs' annual
      # exceptions criteria ("The [annual fourth highest / 98th / 99th
      # percentile based on the available data] exceeds the standard" and
      # the NO2/SO2 relaxed 50%-per-quarter annual-mean criterion).
      tibble::tibble(ok = details$days_ok && details$hours_ok)
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$year) |>
    tidyr::pivot_wider(names_from = "pol", values_from = "ok")
}

## CAAQS Helpers ----------------------------------------------------------
CAAQS_pm25 <- function(obs, thresholds) {
  obs <- obs |>
    dplyr::group_by(year = .data$date |> lubridate::year())
  # Year-level completeness for the annual metrics: PM2.5 GDAD (PN 1483,
  # 2012) sections 4.1.4 and 4.2.4 require "at least 75% valid daily-24hr-
  # PM2.5 in the year" and "at least 60% ... in each calendar quarter".
  # The 2012 document has no exceptions column, so a year failing these
  # criteria contributes no metric values (see CAAQS_completeness()$pm25).
  year_details <- obs |> CAAQS_year_gate_details(CAAQS_completeness()$pm25, "pm25")
  obs |>
    # Hourly means -> daily 24-hr means. PM2.5 GDAD (PN 1483, 2012) section
    # 4.1.1 (Equation 1): the daily 24hr-PM2.5 is the mean of the available
    # 1-hour concentrations ("If at least 18 hours are available, the
    # denominator in Equation 1 will be the number of hours available"), and
    # section 4.1.4 makes the day valid only when "at least 75% (18 hours)
    # of the 1-hour concentrations are available on the given day".
    dplyr::ungroup() |>
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("days")
    ) |>
    dplyr::summarise(
      .groups = "drop",
      # The daily 24hr-PM2.5 is "the daily 24-hour average concentration
      # (in ug/m3) rounded to one decimal place using the procedures
      # specified in Appendix D" (PM2.5 GDAD section 4.1.1); the annual
      # average and metric values are rounded per Appendix D as well
      # (CAAQS_round_gdad()).
      pm25_mean = .data$pm25 |> mean(na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$pm25$daily),
      daily_avail_hours = sum(!is.na(.data$pm25))
    ) |>
    dplyr::filter(.data$daily_avail_hours >= CAAQS_completeness()$pm25$min_hours_of_day) |>
    # Valid daily means -> annual 98th percentile and annual average
    dplyr::group_by(year = lubridate::year(.data$date)) |>
    dplyr::summarise(
      .groups = "drop",
      # Annual 98th percentile via the GDAD percentile ranking approach
      # (section 4.1.2, Steps 1-3): the (N - Trunc(N * 0.98))th highest
      # daily 24hr-PM2.5, with ties repeated in rank order (worked example
      # N = 275 -> 6th highest; Table 3). Footnote 11 forbids software whose
      # percentile procedure differs, so stats::quantile() (type 7) is not
      # compliant.
      perc_98_of_daily_means = .data$pm25_mean |>
        CAAQS_rank_percentile(0.98),
      # Annual average per section 4.2.2 (Equation 3): the mean of the
      # valid daily-24hr-PM2.5 values in the year.
      # "The annual average is to be rounded to one decimal place based on
      # the procedure specified in Appendix D" (PM2.5 GDAD section 4.2.2).
      mean_of_daily_means = mean(.data$pm25_mean, na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$pm25$annual)
    ) |>
    # The year-level completeness gates (sections 4.1.4/4.2.4) decide
    # whether the year's metric values are valid; with no exceptions column
    # in the 2012 GDAD there is no exception path (see
    # CAAQS_completeness()$pm25), so gated-out years contribute nothing.
    # The metric values are the same whether or not the year is gated in,
    # so computing them for every year and keeping the gated-in ones is
    # equivalent to evaluating the metrics on gated-in years only.
    dplyr::left_join(year_details, by = "year") |>
    dplyr::filter(.data$days_ok & .data$hours_ok) |>
    dplyr::select(-dplyr::any_of(
      c("days_ok", "hours_ok", "relaxed_quarters_ok")
    )) |>
    # +3 year averages, +whether standard is met. Per sections 4.1.4 and
    # 4.2.4 a metric value is valid when its annual values (98P or annual
    # average) "are available for at least two of the required three years".
    dplyr::mutate(
      # Metric values are rounded to one decimal place per Appendix D
      # (PM2.5 GDAD sections 4.1.3 and 4.2.3).
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_means |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ) |>
        CAAQS_round_gdad(CAAQS_metric_digits$pm25$`3yr`),
      management_level_daily = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
              thresholds = thresholds$pm25$daily
            )
          }
        ),
      `3yr_mean_of_means` = .data$mean_of_daily_means |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ) |>
        CAAQS_round_gdad(CAAQS_metric_digits$pm25$`3yr`),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_means`[.data$year == y],
              thresholds = thresholds$pm25$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_daily",
      "mean_of_daily_means",
      .after = "3yr_mean_of_perc_98"
    )
}

CAAQS_o3 <- function(obs, thresholds) {
  obs <- obs |>
    dplyr::group_by(year = .data$date |> lubridate::year())
  # Year-level completeness for the annual fourth-highest metric: Ozone
  # GDAD (2021) Table 5-3 (Annual fourth highest O3 Dmax 8-hour): "O3 Dmax
  # 8-hour are available for at least 75% of the days in the period April 1
  # to September 30". Under the annual row's column-3 exception ("The
  # annual fourth highest exceeds the standard") a gated-out year still
  # contributes its annual fourth highest when that value, computed on all
  # available data of the year, exceeds the standard (see
  # CAAQS_completeness()$o3).
  year_details <- obs |> CAAQS_year_gate_details(CAAQS_completeness()$o3, "o3")
  obs |>
    # hourly mean -> 8-hour rolling mean, per the CCME Guidance Document on
    # Achievement Determination for Ozone (2021), eq. 5.1: the O3-8-hr for
    # hour J (J = 1 to 24) is the mean of the O3 1-hour over the 8-hour period
    # ending at that hour and is assigned to that ending hour. N, the number
    # of available O3 1-hour in the period, may be 6, 7 or 8: Table 5-3
    # requires at least six of the eight 1-hour values, so series starts use
    # 6- or 7-hour averages as in the GDAD's January 1 example (first average
    # for the hour 01:00 built from the hours back to 18:00 on December 31).
    # The window ending at hour J occupies the hourly rows labelled
    # [J - 7, J] under hour-ending labels, and the 24 windows of a calendar
    # day are the rolling values at that day's 24 hourly rows.
    dplyr::mutate(
      `8hr_mean_o3` = .data$o3 |>
        handyr::rolling(
          "mean",
          .width = 8,
          .direction = "backward",
          .min_non_na = 6
        )
    ) |>
    dplyr::ungroup() |>
    # 8-hour rolling means -> daily maximum over the 24 windows (J = 1 to 24)
    # ending in the day. Table 5-3 (Ozone Dmax 8-hour): "At least 18 (75%) of
    # the 24 O3-8-hr are available in the day", with the column-3 exception
    # "The O3 Dmax 8-hour exceeds the standard": a deficient day is excluded
    # unless its O3 Dmax 8-hour exceeds the standard (GDAD section 5.3:
    # "Since this O3 Dmax 8-hour exceeds the standard, it will be retained
    # for the selection of the annual fourth highest even though the
    # completeness criterion was not satisfied").
    dplyr::group_by(date = .data$date |> lubridate::floor_date("days")) |>
    dplyr::summarise(
      # The O3-8-hr and, directly obtained from it, the O3 Dmax 8-hour are
      # reported to one decimal place (Ozone GDAD 2021 Table 5-4); the
      # rounding commutes with the maximum, so rounding the daily maximum
      # here covers the rolling averages it is taken over.
      daily_max_8hr_mean_o3 = .data$`8hr_mean_o3` |> handyr::max(na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$o3$annual),
      valid_windows = sum(!is.na(.data$`8hr_mean_o3`)),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$valid_windows >= CAAQS_completeness()$o3$min_hours_of_day |
        .data$daily_max_8hr_mean_o3 >
          CAAQS_red_threshold(lubridate::year(.data$date), thresholds$o3$`8hr`)
    ) |>
    # A table with no rankable rows must stay empty rather than emitting an
    # all-NA annual fourth highest (handyr::max(na.rm = TRUE) on an empty
    # input would warn; the summarise above keeps one all-NA row).
    dplyr::filter(dplyr::n() > 0) |>
    # Daily maxima are ranked for the annual fourth-highest only within the
    # ozone season. Table 5-3 (Annual fourth highest O3 Dmax 8-hour): "O3
    # Dmax 8-hour are available for at least 75% of the days in the period
    # April 1 to September 30"; section 5.3 restricts the completeness
    # requirement to that period because it is "the time of year where the
    # annual fourth highest will likely be recorded in most of Canada".
    # The annual row's column-3 exception ("The annual fourth highest
    # exceeds the standard") is applied to the result below: a year that
    # fails the season days criterion still contributes its fourth highest
    # when that value exceeds the standard.
    dplyr::filter(
      !(lubridate::month(.data$date) %in% c(1:3, 10:12))
    ) |>
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    dplyr::arrange(dplyr::desc(.data$daily_max_8hr_mean_o3)) |>
    dplyr::summarise(
      .groups = "drop",
      fourth_highest_daily_max_8hr_mean_o3 = .data$daily_max_8hr_mean_o3[4]
    ) |>
    # Annual-row exception (Table 5-3, column 3): gated-in years keep
    # their fourth highest; gated-out years keep theirs only when it
    # exceeds the standard (CAAQS_apply_annual_exception()). Years retained
    # by neither are dropped entirely, so the 3-year rolling means below
    # see only the years with a valid annual value.
    dplyr::left_join(year_details, by = "year") |>
    dplyr::mutate(
      fourth_highest_daily_max_8hr_mean_o3 = CAAQS_apply_annual_exception(
        .data$fourth_highest_daily_max_8hr_mean_o3,
        year_ok = .data$days_ok,
        exception = CAAQS_completeness()$o3$annual_metric_exception,
        relaxed_ok = TRUE,
        exceeds_standard = dplyr::coalesce(
          .data$fourth_highest_daily_max_8hr_mean_o3 >
            CAAQS_red_threshold(.data$year, thresholds$o3$`8hr`),
          FALSE
        )
      )
    ) |>
    dplyr::filter(!is.na(.data$fourth_highest_daily_max_8hr_mean_o3)) |>
    dplyr::select(-dplyr::any_of(
      c("days_ok", "hours_ok", "relaxed_quarters_ok")
    )) |>
    # +3 year averages, +whether standard is met. Per GDAD Table 5-3 the
    # metric value may be based on two of the possible three annual fourth
    # highest, so a 3-year window needs at least 2 available years.
    dplyr::mutate(
      # The metric value is reported as a whole number via the GDAD two-step
      # procedure (Ozone GDAD 2021 Table 5-4; worked example in Text Box 2:
      # 62.966... ppb -> 62.9 ppb -> 63 ppb).
      `3yr_mean` = .data$fourth_highest_daily_max_8hr_mean_o3 |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ) |>
        CAAQS_round_gdad(CAAQS_metric_digits$o3$`3yr`),
      management_level_8hr = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean`[.data$year == y],
              thresholds = thresholds$o3$`8hr`
            )
          }
        )
    )
}

CAAQS_no2 <- function(obs, thresholds) {
  obs <- obs |>
    dplyr::group_by(year = .data$date |> lubridate::year())
  # Year-level completeness for both metrics: Nitrogen Dioxide GDAD (2020)
  # Table 5-3 requires the percentile row's days criteria ("The NO2 Dmax
  # 1-hour are available for at least: 1. 75% of the days in a year; and
  # 2. 60% of the days in each calendar quarter") and the annual metric
  # value row's hourly criteria ("1. at least 75% of the NO2 1-hour are
  # available in the year; and 2. at least 60% of the NO2 1-hour are
  # available in each calendar quarter"). Under the annual rows' column-3
  # exceptions ("The 98th percentile based on the available NO2 Dmax 1-hour
  # exceeds the standard"; "1. at least 50% of the NO2 1-hour are available
  # in each calendar quarter; and 2. the annual average exceeds the
  # standard") a gated-out year still contributes the respective metric
  # when those conditions hold (see CAAQS_completeness()$no2).
  year_details <- obs |> CAAQS_year_gate_details(CAAQS_completeness()$no2, "no2")
  obs |>
    # + annual mean
    dplyr::ungroup() |>
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    # The annual metric value is reported to one decimal place (NO2 GDAD
    # 2020 Table 5-4: "Annual metric value (annual average of the NO2 1-
    # hour) ... One decimal place"); the SO2 rule is identical.
    dplyr::mutate(
      annual_mean_of_hourly = .data$no2 |> mean(na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$no2$annual)
    ) |>
    # hourly mean -> daily maxima, keeping only days meeting the daily
    # completeness criterion. Table 5-3 (NO2 Dmax 1-hour): "At least 18 of
    # the 24 (75%) NO2 1-hour are available in the day", with the column-3
    # exception "The NO2 Dmax 1-hour exceeds the standard": a deficient day
    # is excluded unless its daily maximum exceeds the standard. Those daily
    # maxima feed both the annual 98th percentile and (via the metric value
    # criteria) the annual-mean metric.
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      # The NO2 Dmax 1-hour is reported to one decimal place (NO2 GDAD 2020
      # Table 5-4 footnote: "directly obtained from the NO2 1-hour"); the
      # rounding commutes with the ranking used for the 98th percentile.
      daily_max_hourly_no2 = .data$no2 |> handyr::max(na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$no2$annual),
      daily_avail_hours = sum(!is.na(.data$no2))
    ) |>
    dplyr::filter(
      .data$daily_avail_hours >= CAAQS_completeness()$no2$min_hours_of_day |
        .data$daily_max_hourly_no2 >
          CAAQS_red_threshold(lubridate::year(.data$date), thresholds$no2$hourly)
    ) |>
    # daily maxima -> annual 98th percentile. Table 5-3 (Annual 98th
    # percentile of the NO2 Dmax 1-hour): the NO2 Dmax 1-hour must "be
    # available for at least: 1. 75% of the days in a year; and 2. 60% of
    # the days in each calendar quarter". The percentile is computed for
    # every year on all available data, and its validity -- gated-in years,
    # plus gated-out years under the column-3 exception "The 98th
    # percentile based on the available NO2 Dmax 1-hour exceeds the
    # standard" -- is decided below, so the value never depends on the
    # gates.
    dplyr::group_by(
      year = date |> lubridate::year(),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      # Annual 98th percentile via the GDAD percentile ranking approach
      # (Appendix B): the Kth highest daily maximum, K = NDM - trunc(NDM * 0.98)
      perc_98_of_daily_maxima = .data$daily_max_hourly_no2 |>
        CAAQS_rank_percentile(0.98)
    ) |>
    # Annual-row exceptions (Table 5-3, column 3): gated-in years keep
    # their values; gated-out years keep the 98th percentile only when it
    # exceeds the standard, and the annual mean only when every calendar
    # quarter holds at least 50% of its NO2 1-hour AND the annual average
    # exceeds the standard. Years retained by neither are dropped
    # entirely.
    dplyr::left_join(year_details, by = "year") |>
    dplyr::mutate(
      perc_98_of_daily_maxima = CAAQS_apply_annual_exception(
        .data$perc_98_of_daily_maxima,
        year_ok = .data$days_ok,
        exception = CAAQS_completeness()$no2$annual_metric_exception,
        relaxed_ok = TRUE,
        exceeds_standard = dplyr::coalesce(
          .data$perc_98_of_daily_maxima >
            CAAQS_red_threshold(.data$year, thresholds$no2$hourly),
          FALSE
        )
      ),
      annual_mean_of_hourly = CAAQS_apply_annual_exception(
        .data$annual_mean_of_hourly,
        year_ok = .data$hours_ok,
        exception = CAAQS_completeness()$no2$annual_mean_exception,
        relaxed_ok = .data$relaxed_quarters_ok,
        exceeds_standard = dplyr::coalesce(
          .data$annual_mean_of_hourly >
            CAAQS_red_threshold(.data$year, thresholds$no2$annual),
          FALSE
        )
      )
    ) |>
    dplyr::filter(
      !is.na(.data$perc_98_of_daily_maxima) |
        !is.na(.data$annual_mean_of_hourly)
    ) |>
    dplyr::select(-dplyr::any_of(
      c("days_ok", "hours_ok", "relaxed_quarters_ok")
    )) |>
    # +3 year averages, +standard for that year, +whether standard is met.
    # Per the GDAD (Table 5-3) the 1-hour metric value may be based on two of
    # the possible three annual 98th percentiles.
    dplyr::mutate(
      # The 1-hour metric value is reported as a whole number via the GDAD
      # two-step procedure (NO2 GDAD 2020 Table 5-4; worked example in
      # Text Box 3: 62.966... ppb -> 62.9 ppb -> 63 ppb).
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_maxima |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ) |>
        CAAQS_round_gdad(CAAQS_metric_digits$no2$`3yr`),
      # The hourly CAAQS metric is the 3-year average of the annual 98th
      # percentile of daily maximum 1-hour concentrations; the annual CAAQS
      # metric is the annual mean of 1-hour concentrations.
      management_level_hourly = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
              thresholds = thresholds$no2$hourly
            )
          }
        ),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`annual_mean_of_hourly`[.data$year == y],
              thresholds = thresholds$no2$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_hourly",
      .after = "annual_mean_of_hourly"
    )
}

CAAQS_so2 <- function(obs, thresholds) {
  obs <- obs |>
    dplyr::group_by(year = date |> lubridate::year())
  # Year-level completeness for both metrics: Sulphur Dioxide GDAD (2020)
  # Table 5-3 requires the percentile row's days criteria ("The SO2 Dmax
  # 1-hour are available for at least: 1. 75% of the days in a year; and
  # 2. 60% of the days in each calendar quarter") and the annual metric
  # value row's hourly criteria ("1. at least 75% of the SO2 1-hour are
  # available in the year; and 2. at least 60% of the SO2 1-hour are
  # available in each calendar quarter"). Under the annual rows' column-3
  # exceptions ("The 99th percentile based on the available SO2 Dmax 1-hour
  # exceeds the standard"; "1. at least 50% of the SO2 1-hour are available
  # in each calendar quarter; and 2. the annual average exceeds the
  # standard") a gated-out year still contributes the respective metric
  # when those conditions hold (see CAAQS_completeness()$so2).
  year_details <- obs |> CAAQS_year_gate_details(CAAQS_completeness()$so2, "so2")
  obs |>
    # + annual mean
    dplyr::ungroup() |>
    dplyr::group_by(year = date |> lubridate::year()) |>
    dplyr::mutate(annual_mean_of_hourly = .data$so2 |> mean(na.rm = TRUE)) |>
    # hourly mean -> daily maxima, keeping only days meeting the daily
    # completeness criterion. Table 5-3 (SO2 Dmax 1-hour): "At least 18 of
    # the 24 (75%) SO2 1-hour are available in the day", with the column-3
    # exception "The SO2 Dmax 1-hour exceeds the standard": a deficient day
    # is excluded unless its daily maximum exceeds the standard.
    dplyr::group_by(
      date = date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      # The SO2 Dmax 1-hour is reported to one decimal place (SO2 GDAD 2020
      # Table 5-4 footnote: "directly obtained from the SO2 1-hour"); the
      # rounding commutes with the ranking used for the 99th percentile.
      daily_max_hourly_so2 = .data$so2 |> handyr::max(na.rm = TRUE) |>
        CAAQS_round_gdad(CAAQS_metric_digits$so2$annual),
      daily_avail_hours = sum(!is.na(.data$so2))
    ) |>
    dplyr::filter(
      .data$daily_avail_hours >= CAAQS_completeness()$so2$min_hours_of_day |
        .data$daily_max_hourly_so2 >
          CAAQS_red_threshold(lubridate::year(.data$date), thresholds$so2$hourly)
    ) |>
    # daily maxima -> annual 99th percentile. Table 5-3 (Annual 99th
    # percentile of the SO2 Dmax 1-hour): the SO2 Dmax 1-hour must "be
    # available for at least: 1. 75% of the days in a year; and 2. 60% of
    # the days in each calendar quarter". The percentile is computed for
    # every year on all available data, and its validity -- gated-in years,
    # plus gated-out years under the column-3 exception "The 99th
    # percentile based on the available SO2 Dmax 1-hour exceeds the
    # standard" -- is decided below. Uses the GDAD percentile ranking
    # approach (Appendix B): the Kth highest daily maximum,
    # K = NDM - trunc(NDM * 0.99)
    dplyr::group_by(
      year = date |> lubridate::year(),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      perc_99_of_daily_maxima = .data$daily_max_hourly_so2 |>
        CAAQS_rank_percentile(0.99)
    ) |>
    # Annual-row exceptions (Table 5-3, column 3): gated-in years keep
    # their values; gated-out years keep the 99th percentile only when it
    # exceeds the standard, and the annual mean only when every calendar
    # quarter holds at least 50% of its SO2 1-hour AND the annual average
    # exceeds the standard. Years retained by neither are dropped
    # entirely.
    dplyr::left_join(year_details, by = "year") |>
    dplyr::mutate(
      perc_99_of_daily_maxima = CAAQS_apply_annual_exception(
        .data$perc_99_of_daily_maxima,
        year_ok = .data$days_ok,
        exception = CAAQS_completeness()$so2$annual_metric_exception,
        relaxed_ok = TRUE,
        exceeds_standard = dplyr::coalesce(
          .data$perc_99_of_daily_maxima >
            CAAQS_red_threshold(.data$year, thresholds$so2$hourly),
          FALSE
        )
      ),
      annual_mean_of_hourly = CAAQS_apply_annual_exception(
        .data$annual_mean_of_hourly,
        year_ok = .data$hours_ok,
        exception = CAAQS_completeness()$so2$annual_mean_exception,
        relaxed_ok = .data$relaxed_quarters_ok,
        exceeds_standard = dplyr::coalesce(
          .data$annual_mean_of_hourly >
            CAAQS_red_threshold(.data$year, thresholds$so2$annual),
          FALSE
        )
      )
    ) |>
    dplyr::filter(
      !is.na(.data$perc_99_of_daily_maxima) |
        !is.na(.data$annual_mean_of_hourly)
    ) |>
    dplyr::select(-dplyr::any_of(
      c("days_ok", "hours_ok", "relaxed_quarters_ok")
    )) |>
    # +3 year averages, +standard for that year, +whether standard is met.
    # Per the GDAD (Table 5-3) the 1-hour metric value may be based on two of
    # the possible three annual 99th percentiles.
    dplyr::mutate(
      # The 1-hour metric value is reported as a whole number via the GDAD
      # two-step procedure (SO2 GDAD 2020 Table 5-4), as for NO2.
      `3yr_mean_of_perc_99` = .data$perc_99_of_daily_maxima |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ) |>
        CAAQS_round_gdad(CAAQS_metric_digits$so2$`3yr`),
      # The hourly CAAQS metric is the 3-year average of the annual 99th
      # percentile of daily maximum 1-hour concentrations; the annual CAAQS
      # metric is the annual mean of 1-hour concentrations.
      management_level_hourly = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_99`[.data$year == y],
              thresholds = thresholds$so2$hourly
            )
          }
        ),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`annual_mean_of_hourly`[.data$year == y],
              thresholds = thresholds$so2$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_hourly",
      .after = "annual_mean_of_hourly"
    )
}

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
## are aligned vectors (one entry per year with data); `year_ok`,
## `relaxed_ok` and `exceeds_standard` must be NA-free logicals.
CAAQS_apply_annual_exception <- function(
  value, year_ok, exception, relaxed_ok, exceeds_standard
) {
  keep <- year_ok | (exception & relaxed_ok & exceeds_standard)
  value[!keep] <- NA_real_
  value
}

## The Red management level of the CAAQS in force in `year` for the
## pollutant/averaging period addressed by `thresholds` (one of the lists
## in CAAQS_thresholds(), e.g. thresholds$no2$hourly). The Red level is the
## CAAQS itself (CAAQS_thresholds()), so this is the "exceeds the standard"
## value used by the data-completeness exceptions criteria of the GDADs'
## Table 5-3 ("The NO2 Dmax 1-hour exceeds the standard" and similar).
## Returns NA when no CAAQS for that averaging period is yet in force in
## `year`, mirroring CAAQS_meets_standard()'s handling of such years.
## Vectorized over `year` (dplyr::filter() passes whole columns).
CAAQS_red_threshold <- function(year, thresholds) {
  vapply(
    year,
    function(y) {
      mgmt_levels <- thresholds[as.numeric(names(thresholds)) <= y] |> dplyr::last()
      if (length(mgmt_levels) == 0) {
        return(NA_real_)
      }
      unname(mgmt_levels["Red"])
    },
    numeric(1)
  )
}

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

# Current as of 2025-08 (CCME air quality report, https://ccme.ca/en/air-quality-report).
# Threshold values are the management-level band edges of the CCME Guidance
# Document on Air Zone Management (2019), Appendix 2 (Tables A2-1 to A2-4),
# with the effective year as the list name: Red is the CAAQS itself, the
# Orange and Yellow values are the inclusive lower edges shown there ("32 to
# 60 ppb", "21 to 31 ppb", ...), and Green is everything below the Yellow
# lower edge ("< 50 ppb" in the tables). CAAQS_meets_standard() applies the
# comparison semantics the guidance documents state: a CAAQS "is achieved if
# the metric value is less than or equal to the standard" (achievement
# determination GDADs), i.e. Red is strict >, and the band lower edges are
# inclusive. Metric values are rounded per the GDADs (CAAQS_round_gdad())
# before comparison, as Appendix 2 requires ("the metric values for
# comparison to the concentrations must be rounded to the same number of
# digits as the shown concentrations"). The CAAQS O3 metric is defined in the CCME Guidance
# Document on Achievement Determination for Ozone (2021): 3-year average of the
# annual 4th-highest daily maximum 8-hour rolling average. The CAAQS PM2.5
# metrics are defined in the CCME Guidance Document on Achievement
# Determination for Fine Particulate Matter and Ozone (2012, PN 1483),
# sections 4.1 and 4.2: 3-year averages of the annual 98th percentile of
# daily 24-hr means (rank-percentile) and of annual averages of valid daily
# 24-hr means.
CAAQS_thresholds <- function() {
  list(
    pm25 = list(
      daily = list(
        "2015" = c(Red = 28, Orange = 20, Yellow = 11, Green = 0),
        "2020" = c(Red = 27, Orange = 20, Yellow = 11, Green = 0)
      ),
      annual = list(
        "2015" = c(Red = 10, Orange = 6.5, Yellow = 4.1, Green = 0),
        "2020" = c(Red = 8.8, Orange = 6.5, Yellow = 4.1, Green = 0)
      )
    ),
    o3 = list(
      `8hr` = list(
        "2015" = c(Red = 63, Orange = 57, Yellow = 51, Green = 0),
        "2020" = c(Red = 62, Orange = 57, Yellow = 51, Green = 0),
        "2025" = c(Red = 60, Orange = 57, Yellow = 51, Green = 0)
      )
    ),
    no2 = list(
      hourly = list(
        "2020" = c(Red = 60, Orange = 32, Yellow = 21, Green = 0),
        "2025" = c(Red = 42, Orange = 32, Yellow = 21, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 17, Orange = 7.1, Yellow = 2.1, Green = 0),
        "2025" = c(Red = 12, Orange = 7.1, Yellow = 2.1, Green = 0)
      )
    ),
    so2 = list(
      hourly = list(
        "2020" = c(Red = 70, Orange = 51, Yellow = 31, Green = 0),
        "2025" = c(Red = 65, Orange = 51, Yellow = 31, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 5, Orange = 3.1, Yellow = 2.1, Green = 0),
        "2025" = c(Red = 4, Orange = 3.1, Yellow = 2.1, Green = 0)
      )
    )
  )
}
# TODO: implement
CAAQS_objectives <- function(mgmt_levels) {
  c(
    Green = "To maintain good air quality through proactive air management measures to keep clean areas clean.",
    Yellow = "To improve air quality using early and ongoing actions for continuous improvement.",
    Orange = "To improve air quality through active air management and prevent exceedance of the CAAQS.",
    Red = "To reduce pollutant levels below the CAAQS through advanced air management actions."
  )
}
