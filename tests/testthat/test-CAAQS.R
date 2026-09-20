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

test_that("NO2 deficient days are retained only when they exceed the standard per Table 5-3", {
  # Seven valid spike days per year (100 ppb) plus one deficient day per
  # year (1 < 18 available hours). Table 5-3 (NO2 Dmax 1-hour): a deficient
  # day is retained only under the column-3 exception "The NO2 Dmax 1-hour
  # exceeds the standard". The ranking-approach 98th percentile is the 8th
  # highest daily maximum (Appendix B, NDM = 365):
  #   2021: the deficient day holds a 500 ppb value (> the 60 ppb CAAQS in
  #     force), so it is retained: the 8th highest is the smallest of the
  #     eight non-background daily values, i.e. 100 ppb.
  #   2022: the deficient day holds a 50 ppb value (< the standard), so it
  #     is excluded: only the seven spikes exceed the background and the
  #     8th highest is the 1 ppb background. (Retaining it would give 50.)
  #   2023: no deficient day, so the 8th highest is likewise the background.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(1, length(hours))
  spike_hours <- unlist(lapply(2021:2023, function(y) {
    lubridate::ymd_h(paste0(y, "-06-0", 1:7, " 14"))
  }))
  no2[hours %in% spike_hours] <- 100
  # 2021: deficient day EXCEEDING the standard -> retained.
  no2[hours %in% make_hours("2021-07-01 00", "2021-07-01 23")] <- NA
  no2[hours %in% make_hours("2021-07-01 08", "2021-07-01 08")] <- 500
  # 2022: deficient day BELOW the standard -> excluded.
  no2[hours %in% make_hours("2022-07-01 00", "2022-07-01 23")] <- NA
  no2[hours %in% make_hours("2022-07-01 08", "2022-07-01 08")] <- 50

  output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2

  expect_identical(output$perc_98_of_daily_maxima, c(100, 1, 1))

  # Control: an eighth valid spike day in each year raises the 8th highest
  # daily maximum to 100 ppb.
  no2[hours %in% make_hours("2021-07-02 08", "2021-07-02 08")] <- 100
  no2[hours %in% make_hours("2022-07-02 08", "2022-07-02 08")] <- 100
  no2[hours %in% make_hours("2023-07-02 08", "2023-07-02 08")] <- 100

  output <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2

  expect_identical(output$perc_98_of_daily_maxima, c(100, 100, 100))
})

test_that("O3 deficient days are retained only when they exceed the standard per Table 5-3", {
  # Section 5.3 example: "there are less than eighteen O3-8-hr in a given
  # day and the O3 Dmax 8-hour based on the available data is 70 ppb. Since
  # this O3 Dmax 8-hour exceeds the standard, it will be retained for the
  # selection of the annual fourth highest even though the completeness
  # criterion was not satisfied." Table 5-3 (Ozone Dmax 8-hour), column 3:
  # "The O3 Dmax 8-hour exceeds the standard".
  #
  # 2021 background 10 ppb with three valid in-season days at 50, 51 and
  # 52 ppb and two deficient days (8 plateau hours, the other 16 missing,
  # so only 3 of the 24 rolling windows are valid):
  #   2021-05-20 at 70 ppb (> the 60 ppb standard in force): retained.
  #   2021-06-20 at 55 ppb (< the standard): excluded.
  # The ordered daily maxima are 70, 52, 51, 50, 10, ..., so the annual
  # fourth highest is 50 ppb: without the exception the retained day would
  # be dropped (leaving only three rankable values, i.e. an NA fourth
  # highest), and had the deficient 55 ppb day also been retained the
  # fourth highest would be 51 ppb.
  hours <- make_hours("2021-01-01 00", "2021-12-31 23")
  o3 <- rep(10, length(hours))
  for (spike in list(c("2021-05-10", "50"), c("2021-06-10", "51"), c("2021-08-10", "52"))) {
    o3[hours %in% make_hours(paste0(spike[1], " 09"), paste0(spike[1], " 16"))] <-
      as.numeric(spike[2])
  }
  for (day in c("2021-05-20", "2021-06-20")) {
    o3[hours %in% make_hours(paste0(day, " 00"), paste0(day, " 08"))] <- NA
    o3[hours %in% make_hours(paste0(day, " 17"), paste0(day, " 23"))] <- NA
  }
  o3[hours %in% make_hours("2021-05-20 09", "2021-05-20 16")] <- 70
  o3[hours %in% make_hours("2021-06-20 09", "2021-06-20 16")] <- 55

  output <- CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())

  expect_identical(output$fourth_highest_daily_max_8hr_mean_o3, 50)
})

test_that("CAAQS_red_threshold returns the CAAQS in force for a year", {
  th <- CAAQS_thresholds()
  # O3 8-hour: 62 ppb in force from 2020, 60 ppb from 2025.
  expect_equal(CAAQS_red_threshold(2024, th$o3$`8hr`), 62)
  expect_equal(CAAQS_red_threshold(2025, th$o3$`8hr`), 60)
  # Vectorized over year, as dplyr::filter() passes whole columns; NA where
  # no standard is yet in force.
  expect_equal(
    CAAQS_red_threshold(c(2019, 2021, 2026), th$no2$hourly),
    c(NA_real_, 60, 42)
  )
  expect_equal(CAAQS_red_threshold(c(2018, 2020), th$pm25$daily), c(28, 27))
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

test_that("CAAQS_pm25 daily means require 18 of 24 hours per PM2.5 GDAD sections 4.1.1/4.1.4", {
  # 2021 with ten full-day spike values (25-34 ug/m3): the ranking-approach
  # 98th percentile is the 365 - Trunc(0.98 * 365) = 8th highest daily
  # 24hr-PM2.5 (PM2.5 GDAD section 4.1.2), i.e. 27. (Type-7 interpolation
  # would return 27.72, so this also pins the ranking approach.)
  hours <- make_hours("2021-01-01 00", "2021-12-31 23")
  pm25 <- rep(5, length(hours))
  for (i in seq_along(1:10)) {
    day <- c(
      "06-01", "06-15", "07-01", "07-15", "08-01",
      "08-15", "09-01", "09-15", "10-01", "10-15"
    )[i]
    pm25[hours %in% make_hours(paste0("2021-", day, " 00"), paste0("2021-", day, " 23"))] <-
      24 + i
  }

  output <- CAAQS_pm25(
    data.frame(date = hours, pm25 = pm25),
    CAAQS_thresholds()
  )
  expect_equal(output$perc_98_of_daily_means, 27)

  # Blank out 7 hours of the Aug 15 day (30 ug/m3): 17 < 18 valid hours, so
  # the daily 24hr-PM2.5 is invalid (section 4.1.1) and the day is excluded
  # from the ranking: the 98th percentile falls from 27 to the next spike
  # (26), and 30 no longer appears in the year's daily values.
  pm25[hours %in% make_hours("2021-08-15 09", "2021-08-15 15")] <- NA
  output <- CAAQS_pm25(
    data.frame(date = hours, pm25 = pm25),
    CAAQS_thresholds()
  )
  expect_equal(output$perc_98_of_daily_means, 26)
  expect_false(30 %in% unlist(output$perc_98_of_daily_means))
})

test_that("CAAQS_pm25 annual metrics require 75% of valid days and 60% per quarter (PM2.5 GDAD sections 4.1.4/4.2.4)", {
  has <- function(hours, pm25) {
    CAAQS_has_enough_obs(
      data.frame(date = hours, pm25 = pm25),
      CAAQS_completeness()
    )$pm25
  }
  # Complete 2021-2023: every criterion holds.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  pm25 <- rep(5, length(hours))
  expect_identical(has(hours, pm25), c(TRUE, TRUE, TRUE))

  # 2022 with July 1 onward missing: Q3 (and Q4) have 0 valid days
  # (< 60% of the quarter), so 2022 fails completeness. 2023 is emptied
  # entirely by the same assignment and fails as well; 2021 is untouched.
  pm25[hours >= lubridate::ymd_h("2022-07-01 00")] <- NA
  expect_identical(has(hours, pm25), c(TRUE, FALSE, FALSE))

  # Missing 9 days from every month (108 days, 29.6% of the year) keeps
  # every quarter above 60% (about 90% of days each) but fails the
  # year-wide 75%-of-days criterion (257/365 = 70%).
  pm25 <- rep(5, length(hours))
  pm25[as.integer(format(hours, "%d")) <= 9] <- NA
  expect_identical(has(hours, pm25), c(FALSE, FALSE, FALSE))
})

test_that("CAAQS_completeness PM2.5 criteria follow PM2.5 GDAD sections 4.1.4 and 4.2.4", {
  # Daily: "at least 75% (18 hours) of the 1-hour concentrations are
  # available on the given day"; annual (both metrics): "at least 75% valid
  # daily-24hr-PM2.5 in the year" and "at least 60% ... in each calendar
  # quarter". Unlike NO2/SO2 there is no hours-per-year criterion.
  pm25 <- CAAQS_completeness()$pm25
  expect_equal(pm25$min_hours_of_day, 18L)
  expect_equal(pm25$min_days_fraction, 0.75)
  expect_equal(pm25$min_days_fraction_quarters, 0.6)
  expect_null(pm25$min_hours_fraction_year)
  expect_null(pm25$min_hours_fraction_quarters)
  expect_null(pm25$season)
})

test_that("a gated-out year with an exceeding annual fourth highest is retained per the Ozone GDAD Table 5-3 annual-row exception", {
  # 2021: in-season valid days Aug 1 - Sep 30 gone -> 122/183 (66.7%) < 75%,
  # so the year fails the annual criterion, but four plateau days at 70 ppb
  # give an annual fourth highest of 70, exceeding the CAAQS in force (62 ppb
  # for 2021-2024) -> the annual-row exception ("The annual fourth highest
  # exceeds the standard") retains it. 2022 keeps a 45.4% gap but 168/183
  # (91.8%) in-season days -> gated in, fh = 10. 2023 is complete.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c(
    "2021-04-10", "2021-05-10", "2021-06-10", "2021-07-10",
    "2022-06-10", "2022-07-10"
  )) {
    o3[hours %in% make_hours(paste0(day, " 09"), paste0(day, " 16"))] <- 70
  }
  o3[hours >= lubridate::ymd_h("2021-08-01 00") &
    hours < lubridate::ymd_h("2021-10-01 00")] <- NA_real_
  o3[hours >= lubridate::ymd_h("2022-04-01 00") &
    hours < lubridate::ymd_h("2022-05-16 00")] <- NA_real_

  out <- CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  expect_identical(out$year, c(2021, 2022, 2023))
  expect_identical(
    out$fourth_highest_daily_max_8hr_mean_o3,
    c(70, 10, 10)
  )
  # 2023's 3-year window averages 2021 (retained by the exception), 2022 and
  # 2023 -> mean(70, 10, 10) = 30.
  expect_identical(out$`3yr_mean`, c(NA_real_, NA_real_, 30))
})

test_that("a gated-out year below the standard contributes no annual fourth highest", {
  # Same data gaps as the retained case, but plateaus at 50 ppb: the annual
  # fourth highest of 50 does not exceed the CAAQS in force (62 ppb), so the
  # exception does not fire and 2021 contributes nothing.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c(
    "2021-04-10", "2021-05-10", "2021-06-10", "2021-07-10",
    "2022-06-10", "2022-07-10"
  )) {
    o3[hours %in% make_hours(paste0(day, " 09"), paste0(day, " 16"))] <- 50
  }
  o3[hours >= lubridate::ymd_h("2021-08-01 00") &
    hours < lubridate::ymd_h("2021-10-01 00")] <- NA_real_
  o3[hours >= lubridate::ymd_h("2022-04-01 00") &
    hours < lubridate::ymd_h("2022-05-16 00")] <- NA_real_

  out <- CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  expect_identical(out$year, c(2022, 2023))
  expect_identical(out$fourth_highest_daily_max_8hr_mean_o3, c(10, 10))
})

test_that("a gated-out year with an exceeding 98th percentile is retained per the NO2 GDAD Table 5-3 annual-row exception", {
  # 2021: Sep 15 - Dec 31 gone -> 257/365 days (70.4%) < 75% and Q4 has no
  # valid days, so the year fails the percentile row's days criteria, but
  # eight spike days at 100 ppb give an annual 98th percentile of 100
  # (K = NDM - Trunc(NDM * 0.98)), exceeding the CAAQS in force (60 ppb for
  # 2020-2024) -> the exception ("The 98th percentile based on the available
  # NO2 Dmax 1-hour exceeds the standard") retains it. The annual mean of 5
  # ppb does not exceed the standard and Q4 holds no hours, so neither the
  # relaxed 50%-per-quarter criterion nor the annual-mean exception applies
  # and 2021's annual mean is NA. 2022/2023 are gated in.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(5, length(hours))
  spikes <- unlist(lapply(
    2021:2023,
    \(y) lubridate::ymd_h(paste0(y, "-06-0", 1:8, " 14"))
  ))
  no2[hours %in% spikes] <- 100
  no2[hours >= lubridate::ymd_h("2021-09-15 00") &
    hours < lubridate::ymd_h("2022-01-01 00")] <- NA_real_

  out <- CAAQS_no2(data.frame(date = hours, no2 = no2), CAAQS_thresholds())
  expect_identical(out$year, c(2021, 2022, 2023))
  expect_identical(out$perc_98_of_daily_maxima, c(100, 100, 100))
  # 8 spike hours per year lift the complete years' mean to 5 + 95 * 8/8760.
  expect_equal(
    out$annual_mean_of_hourly,
    c(NA_real_, rep(5 + 95 * 8 / 8760, 2))
  )
})

test_that("the relaxed 50%-per-quarter annual-mean exception fires only on exceedance (NO2 GDAD Table 5-3 annual metric value row)", {
  # 2021: Oct 1 - Nov 15 gone (46/92 = 50.0% of Q4's hours retained) -> the
  # 75%/60% annual criteria fail, but the relaxed criterion ("at least 50% of
  # the NO2 1-hour are available in each calendar quarter") holds. With a
  # constant 20 ppb (above the 17 ppb CAAQS in force for 2020-2024) the
  # annual average exceeds the standard, so the annual-metric-value exception
  # retains 2021's mean. 2022/2023 are gated in.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(20, length(hours))
  no2[hours >= lubridate::ymd_h("2021-10-01 00") &
    hours <= lubridate::ymd_h("2021-11-15 23")] <- NA_real_

  out <- CAAQS_no2(data.frame(date = hours, no2 = no2), CAAQS_thresholds())
  expect_identical(out$year, c(2021, 2022, 2023))
  expect_identical(out$annual_mean_of_hourly, c(20, 20, 20))
  # The percentile exception is separate: 2021's 98th percentile of 20 does
  # not exceed the standard, so it contributes no percentile value.
  expect_identical(out$perc_98_of_daily_maxima, c(NA_real_, 20, 20))

  # The same gaps with a below-standard 5 ppb background: the annual average
  # does not exceed the standard, the exception does not fire, and 2021 is
  # dropped entirely.
  no2b <- rep(5, length(hours))
  no2b[hours >= lubridate::ymd_h("2021-10-01 00") &
    hours <= lubridate::ymd_h("2021-11-15 23")] <- NA_real_
  out <- CAAQS_no2(data.frame(date = hours, no2 = no2b), CAAQS_thresholds())
  expect_identical(out$year, c(2022, 2023))
  expect_identical(out$annual_mean_of_hourly, c(5, 5))
})

test_that("PM2.5 has no annual exception: a gated-out year contributes no metric (PM2.5 GDAD, PN 1483)", {
  # The 2012 GDAD's completeness criteria (sections 4.1.4/4.2.4) have no
  # exceptions column, so a year failing them contributes nothing even when
  # its values exceed the standard: 2021 loses Jul 1 - Dec 31 (Q3/Q4 empty),
  # its mean of 20 exceeds the 8.8 ppb CAAQS, but 2021 must not appear.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  pm25 <- rep(20, length(hours))
  pm25[hours >= lubridate::ymd_h("2021-07-01 00") &
    hours < lubridate::ymd_h("2022-01-01 00")] <- NA_real_

  out <- CAAQS_pm25(data.frame(date = hours, pm25 = pm25), CAAQS_thresholds())
  expect_identical(out$year, c(2022, 2023))
  expect_identical(out$perc_98_of_daily_means, c(20, 20))
  expect_identical(out$mean_of_daily_means, c(20, 20))
})

test_that("CAAQS_completeness flags which pollutants carry annual-row exceptions", {
  comp <- CAAQS_completeness()
  expect_true(comp$o3$annual_metric_exception)
  expect_null(comp$o3$annual_mean_exception)
  expect_true(comp$no2$annual_metric_exception)
  expect_true(comp$no2$annual_mean_exception)
  expect_true(comp$so2$annual_metric_exception)
  expect_true(comp$so2$annual_mean_exception)
  # PM2.5 sets the flags to FALSE explicitly: its 2012 GDAD has no
  # exceptions column at all.
  expect_false(comp$pm25$annual_metric_exception)
  expect_false(comp$pm25$annual_mean_exception)
})

test_that("min_completeness is retired: all pollutants follow the GDAD gates", {
  # 61 days missing across the Jun-Jul quarter boundary leaves both
  # quarters above 60% of days (61/91 and 61/92) and the year above 75%
  # (304/365), so the data pass the GDAD criteria for every pollutant and
  # no warning is issued.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  gap <- hours >= lubridate::ymd_h("2022-06-01 00") &
    hours < lubridate::ymd_h("2022-08-01 00")
  pm25 <- rep(5, length(hours))
  pm25[gap] <- NA
  no2 <- rep(1, length(hours))
  no2[gap] <- NA
  expect_no_warning(CAAQS(dates = hours, pm25_1hr_ugm3 = pm25, no2_1hr_ppb = no2))

  # The heuristic argument itself has been removed from the signature.
  expect_error(
    CAAQS(dates = hours, pm25_1hr_ugm3 = pm25, min_completeness = 0.9),
    "unused argument"
  )
})
