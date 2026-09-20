## Per-pollutant CAAQS metric pipelines (PM2.5, O3, NO2, SO2).
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
