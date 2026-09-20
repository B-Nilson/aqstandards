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
  # Four 8-hour plateaus at 100 ppb running 17:00 through 00:00 across the
  # nights of Dec 28-31, 2021. Under the GDAD's end-of-period attribution
  # each full plateau is the window ending 00:00 of the following day, so
  # 2021's daily maxima are 100, 100, 100 and 88.75 (the window ending
  # 23:00 Dec 28: one background hour plus seven plateau hours) and the
  # annual 4th-highest is 88.75. Start-of-period attribution would put all
  # four full plateaus in 2021 and yield a 4th-highest of 100.
  hours <- make_hours("2020-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  for (day in c("2021-12-28", "2021-12-29", "2021-12-30", "2021-12-31")) {
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

# TODO: write test
# test_that("Providing less than 3 years of consecutive data throws an error", {
#
# })

# TODO: write test
# test_that("Providing less than min_completeness*100% hours of data for a pol/year throws a warning", {
#
# })
