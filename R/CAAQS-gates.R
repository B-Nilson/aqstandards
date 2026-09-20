## CAAQS data-completeness gate layer: calendar periods, per-day and
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
