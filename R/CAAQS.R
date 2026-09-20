#' Assess the attainment of the Canadian Ambient Air Quality Standards (CAAQS)
#'
#' @param dates Vector of hourly datetime values corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_1hr_ugm3 (Optional). Vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param o3_1hr_ppb (Optional). Vector of hourly mean ozone (O3) concentrations (ppb).
#' @param no2_1hr_ppb (Optional). Vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb).
#' @param so2_1hr_ppb (Optional). Vector of hourly mean sulphur dioxide (SO2) concentrations (ppb).
#' @param min_completeness Deprecated. Data completeness is now assessed with the
#'   pollutant-specific criteria in the CCME Guidance Documents on Achievement
#'   Determination (see `CAAQS_completeness()`); this argument is ignored except
#'   for PM2.5, whose guidance has not been reviewed and which keeps a uniform
#'   annual hourly-availability gate (0.5 by default).
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
#' interpolation); annual metrics are annual means of hourly concentrations;
#' and the PM2.5 metrics are the 3-year average of the annual 98th percentile
#' of daily means and the 3-year average of annual means. For O3, NO2 and SO2
#' 3-year metric values are computed when at least two of the three annual
#' values are available.
#'
#' Data completeness is assessed with the pollutant-specific criteria of the
#' guidance documents' Table 5-3 (see `CAAQS_completeness()`): annual metric
#' values are reported only for years meeting the applicable daily, annual and
#' calendar-quarter criteria, and hours-per-year requirements are derived from
#' the calendar rather than hardcoded leap-year arithmetic. Hourly datetimes
#' are assumed to label the start of the averaging hour and to be in local
#' standard time.
#'
#' @references
#' \itemize{
#'   \item CCME, Canadian Ambient Air Quality Standards (report page), \url{https://ccme.ca/en/air-quality-report}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Ozone (2021), \url{https://ccme.ca/en/res/gdadforozonecaaqsen.pdf}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Nitrogen Dioxide (2020), \url{https://ccme.ca/en/res/gdadforcaaqsfornitrogendioxide_en1.0.pdf}
#'   \item CCME, Guidance Document on Achievement Determination for Canadian Ambient Air Quality Standards: Sulphur Dioxide (2020), \url{https://ccme.ca/en/res/gdadforcaaqsforsulphurdioxide_en1.0.pdf}
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
  so2_1hr_ppb = NULL,
  min_completeness = 0.5
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
  # pollutant-specific criteria of the CCME GDADs (Table 5-3). The
  # `min_completeness` argument applies only to PM2.5, whose guidance
  # document has not yet been reviewed (see `CAAQS_completeness()`).
  completeness <- CAAQS_completeness()
  completeness$pm25$min_hours_fraction_year <- min_completeness
  has_enough_obs <- CAAQS_has_enough_obs(obs, completeness)

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

  # Drop data for years lacking enough data
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
      is_insufficient_year <- obs$year %in% insufficient_years
      obs[is_insufficient_year, pol] <- NA
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
## 2021; Nitrogen Dioxide 2020; Sulphur Dioxide 2020). The exceptions in
## column 3 ("the [metric] exceeds the standard") are not implemented here;
## deficient values are dropped rather than retained when they exceed.
CAAQS_completeness <- function() {
  list(
    # Ozone GDAD (2021), Table 5-3 (Ozone Dmax 8-hour): "O3 Dmax 8-hour are
    # available for at least 75% of the days in the period April 1 to
    # September 30." The annual fourth-highest ranking is restricted to that
    # season because it is "the time of year where the annual fourth highest
    # will likely be recorded in most of Canada" (GDAD section 5.3).
    o3 = list(
      min_hours_of_day = 18L,
      season = c(start = "04-01", end = "09-30"),
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = NULL,
      min_hours_fraction_year = NULL,
      min_hours_fraction_quarters = NULL,
      min_days_of_quarters = NULL
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
      min_days_of_quarters = NULL
    ),
    # Sulphur Dioxide GDAD (2020), Table 5-3: identical structure to NO2 with
    # SO2 in place of NO2 (SO2 Dmax 1-hour "At least 18 of the 24 (75%) ...
    # available in the day"; annual 99th percentile "75% of the days in a
    # year and 60% of the days in each calendar quarter"; annual metric
    # value "75% of the SO2 1-hour ... in the year and 60% ... in each
    # calendar quarter").
    so2 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = 0.75,
      min_hours_fraction_quarters = 0.6,
      min_days_of_quarters = NULL
    ),
    # PM2.5 GDAD not yet reviewed (issue #3): keep the package's previous
    # uniform annual hourly-availability heuristic for PM2.5.
    pm25 = list(min_hours_fraction_year = 0.5)
  )
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

  # Hours per year and per calendar quarter are derived from the calendar
  # via lubridate's leap-year rule (which handles century years correctly),
  # replacing the previously hardcoded `year %% 4 == 0` leap check.
  yrs <- sort(unique(obs$year))
  stats_year <- data.frame(year = yrs) |>
    dplyr::mutate(
      hours_in_year = 24 * (365 + as.integer(lubridate::leap_year(.data$year)))
    )
  stats_quarter <- data.frame(
    year = rep(yrs, each = 4),
    quarter = paste0("Q", 1:4)
  ) |>
    dplyr::mutate(
      hours_in_quarter = 24 * (
        c(90, 91, 92, 92) +
          as.integer(.data$quarter == "Q1" & lubridate::leap_year(.data$year))
      )
    )
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
  out <- avail |>
    dplyr::group_by(.data$year, .data$pol) |>
    dplyr::group_modify(~ {
      cfg <- completeness[[.y$pol]]
      if (is.null(cfg)) {
        return(tibble::tibble(ok = FALSE))
      }
      ok <- TRUE
      # A day contributes an available daily maximum when it meets the
      # daily criterion of Table 5-3 (e.g. "At least 18 of the 24 (75%)
      # NO2 1-hour are available in the day"); with no daily criterion any
      # observed hour makes the day available. Deficient days are excluded
      # from the metric pipelines (see CAAQS_o3/no2/so2) but do not by
      # themselves make the year incomplete: the annual criteria below are
      # expressed in available days.
      valid_day <- if (!is.null(cfg$min_hours_of_day)) {
        .x$hours >= cfg$min_hours_of_day
      } else {
        .x$hours > 0
      }
      # Scope of the days gate: the season when one is configured (Ozone
      # GDAD Table 5-3: "O3 Dmax 8-hour are available for at least 75% of
      # the days in the period April 1 to September 30"), otherwise the
      # whole year (NO2/SO2 GDADs Table 5-3: "available for at least 75% of
      # the days in a year").
      scope <- valid_day
      if (!is.null(cfg$season)) {
        in_season <-
          .x$monthday >= cfg$season["start"] & .x$monthday <= cfg$season["end"]
        scope <- valid_day & in_season
      }
      if (!is.null(cfg$min_days_fraction)) {
        # The GDAD denominators are the calendar days of the year (or of
        # the fixed April 1 - September 30 season, 183 days whether or not
        # the year is a leap year).
        days_in_scope <- if (!is.null(cfg$season)) {
          183
        } else {
          365 + as.integer(lubridate::leap_year(.y$year))
        }
        ok <- ok && sum(scope) >= days_in_scope * cfg$min_days_fraction
      }
      if (!is.null(cfg$min_days_of_year)) {
        ok <- ok && sum(valid_day) >= cfg$min_days_of_year
      }
      # Annual hourly gate, e.g. "at least 75% of the NO2 1-hour are
      # available in the year" (NO2 and SO2 GDADs Table 5-3, annual metric
      # value criteria).
      if (!is.null(cfg$min_hours_fraction_year)) {
        hours_in_year <- stats_year$hours_in_year[stats_year$year == .y$year]
        ok <- ok && sum(.x$hours) >= cfg$min_hours_fraction_year * hours_in_year
      }
      # Calendar-quarter gates: "60% of the days in each calendar quarter"
      # (percentile criteria) and "60% of the NO2 1-hour are available in
      # each calendar quarter" (annual metric value criteria), with Q1
      # January 1 - March 31 through Q4 October 1 - December 31 (GDADs
      # Table 5-3 footnote).
      if (!is.null(cfg$min_days_fraction_quarters) ||
            !is.null(cfg$min_hours_fraction_quarters) ||
            !is.null(cfg$min_days_of_quarters)) {
        qd <- .x |>
          dplyr::mutate(
            quarter = paste0("Q", lubridate::quarter(.data$date, fiscal_start = 1)),
            valid = valid_day
          ) |>
          dplyr::group_by(.data$quarter) |>
          dplyr::summarise(
            .groups = "drop",
            days = sum(.data$valid),
            hours_avail = sum(.data$hours)
          )
        q <- dplyr::left_join(
          stats_quarter[stats_quarter$year == .y$year, ],
          qd,
          by = "quarter"
        )
        if (!is.null(cfg$min_days_fraction_quarters)) {
          ok <- ok &&
            all(q$days >= q$hours_in_quarter / 24 * cfg$min_days_fraction_quarters)
        }
        if (!is.null(cfg$min_hours_fraction_quarters)) {
          ok <- ok && all(q$hours_avail >= q$hours_in_quarter * cfg$min_hours_fraction_quarters)
        }
        if (!is.null(cfg$min_days_of_quarters)) {
          ok <- ok && all(q$days >= cfg$min_days_of_quarters)
        }
      }
      tibble::tibble(ok = isTRUE(ok))
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$year) |>
    tidyr::pivot_wider(names_from = "pol", values_from = "ok")
}

## CAAQS Helpers ----------------------------------------------------------
CAAQS_pm25 <- function(obs, thresholds) {
  obs |>
    # Hourly mean -> daily mean
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("days")
    ) |>
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      c(mean = \(x) mean(x, na.rm = TRUE))
    )) |>
    # Daily mean -> annual 98th percentile and annual mean
    dplyr::group_by(year = lubridate::year(.data$date)) |>
    dplyr::summarise(
      .groups = "drop",
      perc_98_of_daily_means = .data$pm25_mean |>
        stats::quantile(0.98, na.rm = T) |>
        unname(),
      mean_of_daily_means = mean(.data$pm25_mean, na.rm = TRUE)
    ) |>
    # +3 year averages, +whether standard is met
    dplyr::mutate(
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_means |>
        handyr::rolling("mean", .width = 3, .direction = "backward"),
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
        handyr::rolling("mean", .width = 3, .direction = "backward"),
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
    # 8-hour rolling means -> daily maximum over the 24 windows (J = 1 to 24)
    # ending in the day. Table 5-3 (Ozone Dmax 8-hour): "At least 18 (75%) of
    # the 24 O3-8-hr are available in the day", so days with fewer than 18
    # valid windows contribute no daily maximum to the annual ranking.
    dplyr::group_by(date = .data$date |> lubridate::floor_date("days")) |>
    dplyr::summarise(
      daily_max_8hr_mean_o3 = .data$`8hr_mean_o3` |> handyr::max(na.rm = TRUE),
      valid_windows = sum(!is.na(.data$`8hr_mean_o3`)),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$valid_windows >= 18) |>
    # Daily maxima are ranked for the annual fourth-highest only within the
    # ozone season. Table 5-3 (Annual fourth highest O3 Dmax 8-hour): "O3
    # Dmax 8-hour are available for at least 75% of the days in the period
    # April 1 to September 30"; section 5.3 restricts the completeness
    # requirement to that period because it is "the time of year where the
    # annual fourth highest will likely be recorded in most of Canada"
    # (values outside the season would still count when the season criterion
    # is met or the value exceeds the standard -- an exceptions rule not
    # implemented here; see CAAQS_completeness()).
    dplyr::filter(
      !(lubridate::month(.data$date) %in% c(1:3, 10:12))
    ) |>
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    dplyr::arrange(dplyr::desc(.data$daily_max_8hr_mean_o3)) |>
    dplyr::summarise(
      .groups = "drop",
      fourth_highest_daily_max_8hr_mean_o3 = .data$daily_max_8hr_mean_o3[4]
    ) |>
    # +3 year averages, +whether standard is met. Per GDAD Table 5-3 the
    # metric value may be based on two of the possible three annual fourth
    # highest, so a 3-year window needs at least 2 available years.
    dplyr::mutate(
      `3yr_mean` = .data$fourth_highest_daily_max_8hr_mean_o3 |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ),
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
  obs |>
    # + annual mean
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    dplyr::mutate(annual_mean_of_hourly = .data$no2 |> mean(na.rm = TRUE)) |>
    # hourly mean -> daily maxima, keeping only days meeting the daily
    # completeness criterion. Table 5-3 (NO2 Dmax 1-hour): "At least 18 of
    # the 24 (75%) NO2 1-hour are available in the day"; those daily maxima
    # feed both the annual 98th percentile and (via the metric value
    # criteria) the annual-mean metric.
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      daily_max_hourly_no2 = .data$no2 |> handyr::max(na.rm = TRUE),
      daily_avail_hours = sum(!is.na(.data$no2))
    ) |>
    dplyr::filter(.data$daily_avail_hours >= CAAQS_completeness()$no2$min_hours_of_day) |>
    # daily maxima -> annual 98th percentile. Table 5-3 (Annual 98th
    # percentile of the NO2 Dmax 1-hour): the NO2 Dmax 1-hour must "be
    # available for at least: 1. 75% of the days in a year; and 2. 60% of
    # the days in each calendar quarter", so the percentile is NA otherwise.
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
    # +3 year averages, +standard for that year, +whether standard is met.
    # Per the GDAD (Table 5-3) the 1-hour metric value may be based on two of
    # the possible three annual 98th percentiles.
    dplyr::mutate(
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_maxima |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ),
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
  obs |>
    # + annual mean
    dplyr::group_by(year = date |> lubridate::year()) |>
    dplyr::mutate(annual_mean_of_hourly = .data$so2 |> mean(na.rm = TRUE)) |>
    # hourly mean -> daily maxima, keeping only days meeting the daily
    # completeness criterion. Table 5-3 (SO2 Dmax 1-hour): "At least 18 of
    # the 24 (75%) SO2 1-hour are available in the day".
    dplyr::group_by(
      date = date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      daily_max_hourly_so2 = .data$so2 |> handyr::max(na.rm = TRUE),
      daily_avail_hours = sum(!is.na(.data$so2))
    ) |>
    dplyr::filter(.data$daily_avail_hours >= CAAQS_completeness()$so2$min_hours_of_day) |>
    # daily maxima -> annual 99th percentile. Table 5-3 (Annual 99th
    # percentile of the SO2 Dmax 1-hour): the SO2 Dmax 1-hour must "be
    # available for at least: 1. 75% of the days in a year; and 2. 60% of
    # the days in each calendar quarter", so the percentile is NA otherwise.
    # Uses the GDAD percentile ranking approach (Appendix B): the Kth highest
    # daily maximum, K = NDM - trunc(NDM * 0.99)
    dplyr::group_by(
      year = date |> lubridate::year(),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      perc_99_of_daily_maxima = .data$daily_max_hourly_so2 |>
        CAAQS_rank_percentile(0.99)
    ) |>
    # +3 year averages, +standard for that year, +whether standard is met.
    # Per the GDAD (Table 5-3) the 1-hour metric value may be based on two of
    # the possible three annual 99th percentiles.
    dplyr::mutate(
      `3yr_mean_of_perc_99` = .data$perc_99_of_daily_maxima |>
        handyr::rolling(
          "mean", .width = 3, .direction = "backward", .min_non_na = 2
        ),
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

CAAQS_meets_standard <- function(year, metric, thresholds) {
  mgmt_levels <- thresholds[as.numeric(names(thresholds)) <= year] |>
    dplyr::last()
  if (length(mgmt_levels) == 0) {
    return(NA)
  }
  # Calculate CAAQS attainment
  attainment <- mgmt_levels |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      .show_progress = FALSE,
      \(lvl) metric > lvl
    )
  attainment <- attainment |>
    apply(1, \(x) handyr::min(which(x), na.rm = TRUE))
  attainment[!is.na(attainment)] <- names(mgmt_levels)[
    attainment[!is.na(attainment)]
  ]
  return(attainment)
}

# Current as of 2025-08 (CCME air quality report, https://ccme.ca/en/air-quality-report).
# Threshold values are the Red management level (= the CAAQS itself, with the
# effective year as the list name); Yellow/Orange use a 0.01 offset to emulate
# right-open (inclusive-Red) bins with strict > comparisons in
# CAAQS_meets_standard(). The CAAQS O3 metric is defined in the CCME Guidance
# Document on Achievement Determination for Ozone (2021): 3-year average of the
# annual 4th-highest daily maximum 8-hour rolling average.
CAAQS_thresholds <- function() {
  list(
    pm25 = list(
      daily = list(
        "2015" = c(Red = 28, Orange = 19, Yellow = 10.01, Green = 0),
        "2020" = c(Red = 27, Orange = 19, Yellow = 10.01, Green = 0)
      ),
      annual = list(
        "2015" = c(Red = 10, Orange = 6.41, Yellow = 4.01, Green = 0),
        "2020" = c(Red = 8.8, Orange = 6.41, Yellow = 4.01, Green = 0)
      )
    ),
    o3 = list(
      `8hr` = list(
        "2015" = c(Red = 63, Orange = 56.01, Yellow = 50.01, Green = 0),
        "2020" = c(Red = 62, Orange = 56.01, Yellow = 50.01, Green = 0),
        "2025" = c(Red = 60, Orange = 56.01, Yellow = 50.01, Green = 0)
      )
    ),
    no2 = list(
      hourly = list(
        "2020" = c(Red = 60, Orange = 31.01, Yellow = 20.01, Green = 0),
        "2025" = c(Red = 42, Orange = 31.01, Yellow = 20.01, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 17, Orange = 7.01, Yellow = 2.01, Green = 0),
        "2025" = c(Red = 12, Orange = 7.01, Yellow = 2.01, Green = 0)
      )
    ),
    so2 = list(
      hourly = list(
        "2020" = c(Red = 70, Orange = 50.01, Yellow = 30.01, Green = 0),
        "2025" = c(Red = 65, Orange = 50.01, Yellow = 30.01, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 5, Orange = 3.01, Yellow = 2.01, Green = 0),
        "2025" = c(Red = 4, Orange = 3.01, Yellow = 2.01, Green = 0)
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
