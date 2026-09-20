make_hours <- function(start, end) {
  seq(lubridate::ymd_h(start), lubridate::ymd_h(end), "1 hours")
}

test_that("CAAQS returns expected output", {
  obs <- data.frame(
    date = seq(
      lubridate::ymd_h("2020-01-01 00"),
      lubridate::ymd_h("2023-12-31 23"),
      "1 hours"
    ),
    pm25 = 100,
    o3 = 15,
    no2 = 1,
    so2 = 30
  )

  output <- CAAQS(
    dates = obs$date,
    pm25_1hr_ugm3 = obs$pm25,
    no2_1hr_ppb = obs$no2,
    o3_1hr_ppb = obs$o3,
    so2_1hr_ppb = obs$so2
  )
  expect_snapshot(output)
})

test_that("CAAQS_rank_percentile follows the GDAD percentile ranking approach", {
  # GDAD Appendix B: Kth highest = NDM - Truncated(NDM * p). For NO2
  # (p = 0.98): NDM = 355 -> 8th highest (GDAD worked example); NDM = 360 ->
  # trunc(352.8) = 352 -> 8th highest. For SO2 (p = 0.99): NDM = 365 ->
  # 4th highest; NDM = 100 -> 1st highest (100 * 0.99 truncates to 99).
  set.seed(1)
  x <- stats::runif(365, 0, 100)
  expect_identical(
    CAAQS_rank_percentile(x, 0.98),
    sort(x, decreasing = TRUE)[8]
  )
  expect_identical(
    CAAQS_rank_percentile(x[1:355], 0.98),
    sort(x[1:355], decreasing = TRUE)[8]
  )
  expect_identical(
    CAAQS_rank_percentile(x, 0.99),
    sort(x, decreasing = TRUE)[4]
  )
  # Ties are repeated in rank order, as the GDAD requires.
  ties <- c(5, 9, 9, 9, 2, 1)
  expect_identical(CAAQS_rank_percentile(ties, 0.98), 9)
  # No interpolation: a two-value input returns an actual data value.
  expect_identical(CAAQS_rank_percentile(c(3, 7), 0.98), 7)
  expect_identical(CAAQS_rank_percentile(numeric(0), 0.98), NA_real_)
})

test_that("O3 daily max uses all 8-hour rolling windows, not non-overlapping blocks", {
  # Five years of hourly data at a 10 ppb background, with four elevated days
  # per year holding an exact 8-hour plateau (09:00-16:00) so that one rolling
  # window matches the plateau exactly. The daily maxima are therefore
  # 58, 59, 61, 62 ppb per year and the annual 4th-highest is 58 ppb.
  # Non-overlapping floor_date("8 hours") blocks straddle the plateaus and
  # understate the maxima (largest block mean is 54.75 ppb), so the old
  # implementation returned a 4th-highest below 58 and a lower management
  # level.
  hours <- make_hours("2021-01-01 00", "2025-12-31 23")
  o3 <- rep(10, length(hours))
  for (year in 2021:2025) {
    for (day in c("06-10", "07-15", "08-20", "09-25")) {
      o3[hours %in% make_hours(paste0(year, "-", day, " 09"), paste0(year, "-", day, " 16"))] <-
        list("06-10" = 58, "07-15" = 59, "08-20" = 61, "09-25" = 62)[[day]]
    }
  }

  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3

  expect_identical(output$year, c(2021, 2022, 2023, 2024, 2025))
  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    rep(58, 5)
  )
  # Per the GDAD the metric value may be based on two of the three annual
  # fourth highest, so 2022 (one available year in its 3-year window) is NA
  # and 2023 onward are computable.
  expect_identical(output$`3yr_mean`, c(NA_real_, NA_real_, 58, 58, 58))
  # 58 ppb exceeds the Orange bound (56.01) but not the Red CAAQS (62/60).
  expect_identical(
    output$management_level_8hr,
    c(NA, NA, "Orange", "Orange", "Orange")
  )
})

test_that("O3 8-hour windows are attributed to their ending hour per the GDAD", {
  # Three 8-hour plateaus at 100 ppb running 17:00 through 00:00 across the
  # nights of Apr 27-29, 2021 (in season, so the days rank). Under the
  # GDAD's end-of-period attribution each full plateau is the window ending
  # 00:00 of the following day, so Apr 28, 29 and 30 each have a 100 ppb
  # daily maximum, while Apr 27's maximum is 88.75 (the window ending
  # 23:00: one background hour plus seven plateau hours) and the annual
  # 4th-highest is 88.75. Start-of-period attribution would put the three
  # full plateaus' windows in Apr 27-29 instead, making the 4th-highest
  # 100 ppb.
  hours <- make_hours("2020-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c("2021-04-27", "2021-04-28", "2021-04-29")) {
    o3[hours %in% make_hours(
      paste0(day, " 17"),
      paste0(as.character(as.Date(day) + 1), " 00")
    )] <- 100
  }

  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3

  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    c(10, 88.75, 10, 10)
  )
  expect_identical(
    output$`3yr_mean`,
    c(NA, NA, (10 + 88.75 + 10) / 3, (88.75 + 10 + 10) / 3)
  )
})

test_that("NO2 hourly management level uses the 98th percentile metric, annual uses the annual mean", {
  # Five years: 5 ppb background with one 100 ppb spike hour on each of nine
  # days per year. The daily maxima are 100 ppb on the spike days, so with
  # complete data the ranking-approach 98th percentile is exactly the 8th
  # highest daily maximum (100 ppb) and the 3-year average is 100 ppb, while
  # the annual mean of hourly values stays near 5 ppb. The two management
  # levels must therefore differ: Red on the hourly metric, Yellow on the
  # annual metric.
  hours <- make_hours("2021-01-01 00", "2025-12-31 23")
  no2 <- rep(5, length(hours))
  spike_hours <- unlist(lapply(2021:2025, function(y) {
    lubridate::ymd_h(paste0(y, "-06-0", 1:9, " 14"))
  }))
  no2[hours %in% spike_hours] <- 100

  output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2

  expect_gt(min(output$annual_mean_of_hourly), 5)
  expect_lt(max(output$annual_mean_of_hourly), 5.1)
  expect_equal(
    output$`3yr_mean_of_perc_98`,
    c(NA_real_, NA_real_, 100, 100, 100)
  )
  expect_identical(
    output$management_level_hourly,
    c(NA, NA, "Red", "Red", "Red")
  )
  expect_identical(
    output$management_level_annual,
    rep("Yellow", 5)
  )
})

test_that("SO2 hourly management level uses the 99th percentile metric, annual uses the annual mean", {
  # Five years: 1 ppb background with one 80 ppb spike hour on each of five
  # days per year. The daily maxima are 80 ppb on the spike days, so with
  # complete data the ranking-approach 99th percentile is exactly the 4th
  # highest daily maximum (80 ppb) and the 3-year average is 80 ppb, while
  # the annual mean of hourly values stays near 1 ppb: Red on the hourly
  # metric, Green on the annual metric.
  hours <- make_hours("2021-01-01 00", "2025-12-31 23")
  so2 <- rep(1, length(hours))
  spike_hours <- unlist(lapply(2021:2025, function(y) {
    lubridate::ymd_h(paste0(y, "-06-0", 1:5, " 03"))
  }))
  so2[hours %in% spike_hours] <- 80

  output <- CAAQS(dates = hours, so2_1hr_ppb = so2)$so2

  expect_equal(
    output$`3yr_mean_of_perc_99`,
    c(NA_real_, NA_real_, 80, 80, 80)
  )
  expect_identical(
    output$management_level_hourly,
    c(NA, NA, "Red", "Red", "Red")
  )
  expect_identical(
    output$management_level_annual,
    rep("Green", 5)
  )
})

test_that("hours-per-year requirements are derived from the calendar, not a leap-year modulus", {
  # Probed against exact calendar boundaries with a single annual-hours
  # criterion (75%), the analogue of the NO2/SO2 annual metric gate. The
  # previous `year %% 4` rule treats 2024 and 2100 alike (both divisible
  # by 4); lubridate's leap_year() gives 2024 = 8784 hours and 2100
  # (divisible by 100, not 400) = 8760 hours.
  cfg <- list(pm25 = list(min_hours_fraction_year = 0.75))

  # 2024 with 6587 of 8784 hours available: one hour short of 75%, so the
  # year must fail. A non-leap 8760-hour denominator would wrongly pass it
  # (6587/8760 = 75.2%).
  hours <- make_hours("2024-01-01 00", "2024-12-31 23")
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours[seq_len(6587)], pm25 = 1),
      completeness = cfg
    )$pm25,
    FALSE
  )
  # 2024 with 6588 of 8784 hours: exactly 75%, passes.
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours[seq_len(6588)], pm25 = 1),
      completeness = cfg
    )$pm25,
    TRUE
  )

  # 2100 is not a leap year despite being divisible by 4: with 6577 of
  # 8760 hours available (75.1%) the year must pass. The old `year %% 4`
  # rule used an 8784-hour denominator and would wrongly fail it
  # (6577/8784 = 74.9%).
  hours <- make_hours("2100-01-01 00", "2100-12-31 23")
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours[seq_len(6577)], pm25 = 1),
      completeness = cfg
    )$pm25,
    TRUE
  )
})

test_that("NO2 annual percentile gate requires 75% of days and 60% of each quarter", {
  # Complete years are valid...
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(1, length(hours))
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours, no2 = no2),
      completeness = CAAQS_completeness()
    )$no2,
    c(TRUE, TRUE, TRUE)
  )

  # ...30 days missing from Q4 alone keeps every criterion satisfied: Q4
  # retains 62/92 days (67%) and the year 335/365 days (92%)...
  no2 <- rep(1, length(hours))
  no2[hours >= lubridate::ymd_h("2021-10-01 00") &
        hours < lubridate::ymd_h("2021-10-31 00")] <- NA
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours, no2 = no2),
      completeness = CAAQS_completeness()
    )$no2,
    c(TRUE, TRUE, TRUE)
  )

  # ...but 40 days missing from Q4 (52/92 = 57% of Q4 days left) fails the
  # 60%-per-quarter days criterion even though the year-wide 75% criterion
  # still holds (325/365 = 89%).
  no2 <- rep(1, length(hours))
  no2[hours >= lubridate::ymd_h("2021-10-01 00") &
        hours < lubridate::ymd_h("2021-11-10 00")] <- NA
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours, no2 = no2),
      completeness = CAAQS_completeness()
    )$no2,
    c(FALSE, TRUE, TRUE)
  )

  # ...and 9 days missing from every month (108 days, 29.6% of the year)
  # keeps every quarter above 60% (about 90% of days each) but fails the
  # year-wide 75%-of-days criterion (257/365 = 70%).
  no2 <- rep(1, length(hours))
  no2[as.integer(format(hours, "%d")) <= 9] <- NA
  expect_identical(
    CAAQS_has_enough_obs(
      obs = data.frame(date = hours, no2 = no2),
      completeness = CAAQS_completeness()
    )$no2,
    c(FALSE, FALSE, FALSE)
  )
})

test_that("deficient days are excluded from NO2 daily maxima per Table 5-3", {
  # Seven spike days per year (100 ppb) plus one day with a 500 ppb spike in
  # its only available hour (the other 23 hours missing): with 1 < 18 hours
  # available the day is deficient and its maximum must not enter the
  # ranking. The ranking-approach 98th percentile is the 8th highest daily
  # maximum: with the deficient day excluded no eighth value above the
  # background exists, so the percentile stays at the 1 ppb background (had
  # the deficient day been ranked, the 8th highest would be 500 ppb). An
  # eighth VALID spike day lifts the percentile to 100 ppb, confirming the
  # metric itself is sound.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(1, length(hours))
  spike_hours <- unlist(lapply(2021:2023, function(y) {
    lubridate::ymd_h(paste0(y, "-06-0", 1:7, " 14"))
  }))
  no2[hours %in% spike_hours] <- 100
  no2[hours %in% make_hours("2021-07-01 00", "2021-07-01 23")] <- NA
  no2[hours %in% make_hours("2021-07-01 08", "2021-07-01 08")] <- 500

  output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2

  expect_identical(output$perc_98_of_daily_maxima, c(1, 1, 1))

  # Control: an eighth valid spike day in each year raises the 8th highest
  # daily maximum to 100 ppb.
  no2[hours %in% make_hours("2021-07-02 08", "2021-07-02 08")] <- 100
  no2[hours %in% make_hours("2022-07-02 08", "2022-07-02 08")] <- 100
  no2[hours %in% make_hours("2023-07-02 08", "2023-07-02 08")] <- 100

  output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2

  expect_identical(output$perc_98_of_daily_maxima, c(100, 100, 100))
})

test_that("O3 daily maxima require 18 of 24 valid rolling windows per Table 5-3", {
  # Each of 2021 and 2022 has three valid elevated days (62 ppb plateaus
  # 09:00-16:00, which do not spill into the next day) plus an April 20
  # holding a 55 ppb plateau:
  #   2021-04-20: 55 ppb over 00:00-11:00 only, hours 12:00-23:00 missing.
  #     Only the 14 windows ending 00:00-13:00 are valid (from 14:00 on they
  #     contain 3+ missing hours), so the day has 14 < 18 valid windows, is
  #     deficient, and is excluded: its 55 ppb maximum never ranks, and no
  #     evening values spill into Apr 21. The annual 4th-highest stays at
  #     the background 10 ppb.
  #   2022-04-20: the same 55 ppb over all 24 hours, so all 24 windows are
  #     valid, the day IS ranked (its 55 ppb max is 4th behind the three 62
  #     ppb days; the 49.375 ppb window spilling into Apr 21 ranks below
  #     it), and the 4th-highest is 55 ppb.
  # Availability alone therefore changes the annual 4th-highest from 55 to
  # 10 ppb.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c("04-05", "05-10", "06-15")) {
    o3[hours %in% make_hours(paste0("2021-", day, " 09"), paste0("2021-", day, " 16"))] <- 62
    o3[hours %in% make_hours(paste0("2022-", day, " 09"), paste0("2022-", day, " 16"))] <- 62
  }
  o3[hours %in% make_hours("2021-04-20 00", "2021-04-20 11")] <- 55
  o3[hours %in% make_hours("2021-04-20 12", "2021-04-20 23")] <- NA
  o3[hours %in% make_hours("2022-04-20 00", "2022-04-20 23")] <- 55

  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3

  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    c(10, 55, 10)
  )
})

test_that("O3 ranking is restricted to the April 1 - September 30 season", {
  # With plateaus only in March and October, no in-season daily maxima
  # exist, so no annual fourth-highest can be computed. Including the
  # off-season days (the previous behaviour) would rank 58 ppb.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c("2021-03-15", "2021-10-15", "2022-03-15", "2022-10-15", "2023-03-15", "2023-10-15")) {
    o3[hours %in% make_hours(paste0(day, " 09"), paste0(day, " 16"))] <- 58
  }

  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3

  # The off-season plateau days are excluded from the ranking, so the
  # annual fourth-highest falls back to the in-season background days
  # (10 ppb) instead of 58 ppb as under the previous all-year ranking.
  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    c(10, 10, 10)
  )

  # Moving the same plateaus into the season makes them the ranked maxima:
  # the season restriction changes the result.
  hours2 <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3_2 <- rep(10, length(hours2))
  for (day in c("2021-05-15", "2021-09-15", "2022-05-15", "2022-09-15", "2023-05-15", "2023-09-15")) {
    o3_2[hours2 %in% make_hours(paste0(day, " 09"), paste0(day, " 16"))] <- 58
  }

  output2 <- CAAQS(dates = hours2, o3_1hr_ppb = o3_2)$o3

  # Two in-season elevated days per year: the fourth-highest of the
  # ordered daily maxima (58, 58, 10, 10, ...) is 10 ppb.
  expect_identical(
    output2$fourth_highest_daily_max_8hr_mean_o3,
    c(10, 10, 10)
  )
  # With four in-season elevated days the 4th-highest is the elevated
  # level, confirming the season days enter the ranking.
  o3_3 <- rep(10, length(hours2))
  for (day in c("05-15", "06-15", "08-15", "09-15")) {
    o3_3[hours2 %in% make_hours(paste0("2021-", day, " 09"), paste0("2021-", day, " 16"))] <- 58
    o3_3[hours2 %in% make_hours(paste0("2022-", day, " 09"), paste0("2022-", day, " 16"))] <- 58
    o3_3[hours2 %in% make_hours(paste0("2023-", day, " 09"), paste0("2023-", day, " 16"))] <- 58
  }
  output3 <- CAAQS(dates = hours2, o3_1hr_ppb = o3_3)$o3
  expect_identical(
    output3$fourth_highest_daily_max_8hr_mean_o3,
    rep(58, 3)
  )
})

test_that("a polluted day outside the O3 season is excluded even when the season criterion holds", {
  # Section 5.3: a fourth-highest recorded January-March or October-December
  # counts only if the season days criterion is met or the value exceeds the
  # standard (exceptions not implemented). Here the season criterion holds
  # and the off-season plateau (58 ppb) is below the standard, so it must be
  # ignored in favour of the in-season maxima.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  o3[hours %in% make_hours("2021-01-15 09", "2021-01-15 16")] <- 58
  for (day in c("06-10", "07-15", "08-20", "09-25")) {
    o3[hours %in% make_hours(paste0("2021-", day, " 09"), paste0("2021-", day, " 16"))] <-
      list("06-10" = 50, "07-15" = 51, "08-20" = 52, "09-25" = 53)[[day]]
  }
  for (day in c("06-10", "07-15", "08-20", "09-25")) {
    o3[hours %in% make_hours(paste0("2022-", day, " 09"), paste0("2022-", day, " 16"))] <-
      list("06-10" = 50, "07-15" = 51, "08-20" = 52, "09-25" = 53)[[day]]
  }
  for (day in c("06-10", "07-15", "08-20", "09-25")) {
    o3[hours %in% make_hours(paste0("2023-", day, " 09"), paste0("2023-", day, " 16"))] <-
      list("06-10" = 50, "07-15" = 51, "08-20" = 52, "09-25" = 53)[[day]]
  }

  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3

  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    c(50, 50, 50)
  )
})

test_that("missing years no longer propagate NA into the 3-consecutive-years check", {
  # Data spanning 2021-2024 with 2022 absent entirely: before the fix the
  # lag-based check produced NA windows and errored with
  # "Cannot calculate CAAQS..." even though 2021 and 2023-2024 form the
  # required three consecutive complete years.
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
  o3 <- rep(15, length(hours))
  gap <- hours >= lubridate::ymd_h("2022-01-01 00") &
    hours < lubridate::ymd_h("2023-01-01 00")
  expect_true(any(gap))
  hours <- hours[!gap]
  o3 <- o3[!gap]

  expect_no_error(CAAQS(dates = hours, o3_1hr_ppb = o3))
  output <- CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
  expect_identical(
    output$fourth_highest_daily_max_8hr_mean_o3,
    c(15, 15, 15)
  )
})

test_that("an incomplete year invalidates the metrics that depend on it", {
  # 2024 misses all of Q4 (25.5% of the year's hours): the GDAD criteria
  # fail the year and its data are dropped, so 2024 reports no percentile,
  # while 2023 (whose 3-year window includes the unusable year) can still
  # average 2022 and 2023 per the 2-of-3 metric rule. 2021-2023 remain
  # three consecutive complete years, so the call does not error.
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
  no2 <- rep(1, length(hours))
  no2[hours >= lubridate::ymd_h("2024-10-01 00")] <- NA

  expect_warning(
    output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2,
    "Insufficient data"
  )
  # Years that fail completeness are dropped entirely, so their rows do not
  # appear in the output tibble.
  expect_identical(output$year, c(2021, 2022, 2023))
  expect_identical(output$perc_98_of_daily_maxima, c(1, 1, 1))
  expect_identical(output$`3yr_mean_of_perc_98`, c(NA_real_, NA_real_, 1))
  expect_identical(output$management_level_hourly, c(NA, NA, "Green"))
})

test_that("min_completeness now applies only to PM2.5, whose guidance is unreviewed", {
  hours <- make_hours("2021-01-01 00", "2025-12-31 23")
  pm25 <- rep(5, length(hours))
  pm25[hours >= lubridate::ymd_h("2022-03-01 00") &
         hours < lubridate::ymd_h("2022-05-01 00")] <- NA
  # ~16.4% of 2022 missing: passes the 0.5 default but fails a 0.9 gate,
  # which drops 2022 with a warning (2023-2025 remain three consecutive
  # complete years, so no error).
  expect_warning(
    CAAQS(dates = hours, pm25_1hr_ugm3 = pm25, min_completeness = 0.9),
    "Insufficient data"
  )
  expect_no_warning(
    CAAQS(dates = hours, pm25_1hr_ugm3 = pm25)
  )
  # For NO2 the argument is ignored: 16.4% of 2022 missing passes the fixed
  # GDAD gates regardless of min_completeness, so no warning is issued even
  # at 0.99.
  no2 <- rep(1, length(hours))
  no2[hours >= lubridate::ymd_h("2022-03-01 00") &
        hours < lubridate::ymd_h("2022-05-01 00")] <- NA
  expect_no_warning(
    CAAQS(dates = hours, no2_1hr_ppb = no2, min_completeness = 0.99)
  )
})
