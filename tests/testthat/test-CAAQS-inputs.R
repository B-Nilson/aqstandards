# Issue #4 matrix gaps: input handling. Three groups pin behavior the
# GDADs do not legislate (non-contiguous input dates, explicit NA inputs,
# and input validation), so the expectations are contracts established by
# probing current behavior, not GDAD citations. Where a GDAD rule *does*
# govern (the gates applied to filled rows), the test cites it.
#
# The scenario definitions are shared with the regression-fixture
# specification: data-raw/CAAQS-regression-fixtures.R sources the same
# helpers and carries the same scenarios; the anti-rot test keeps the two
# in sync.

test_that("non-contiguous input dates: absent rows are filled and gated, present rows produce hand-checkable metrics", {
  # 2022 supplies only four isolated days (2022-01-15, 2022-03-03,
  # 2022-07-09, 2022-11-28) of hourly NO2 = 40 ppb; 2021 and 2023 are
  # complete at the same value. Absent dates are not errors: the pipeline
  # fills them as missing. The NO2 GDAD Table 5-3 gates then do the work:
  # 4/365 days = 1.1% of days, and every quarter holds exactly 1 day
  # (1/90 or 1/92 = ~1%), so 2022 fails the 75%-of-days and 60%-per-quarter
  # days criteria and contributes no metric rows.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  no2 <- rep(40, length(hours))
  no2[hours >= lubridate::ymd_h("2022-01-01 00") &
    hours < lubridate::ymd_h("2023-01-01 00")] <- NA_real_
  for (day in c("2022-01-15", "2022-03-03", "2022-07-09", "2022-11-28")) {
    no2[hours %in% make_hours(paste0(day, " 00"), paste0(day, " 23"))] <- 40
  }

  out <- CAAQS_no2(data.frame(date = hours, no2 = no2), CAAQS_thresholds())
  expect_identical(out$year, c(2021, 2023))
  expect_identical(out$perc_98_of_daily_maxima, c(40, 40))
  expect_identical(out$annual_mean_of_hourly, c(40, 40))

  # The wrapper reports the same shape: 2022 is warned about, not an error.
  expect_warning(
    result <- CAAQS(
      dates = hours, no2_1hr_ppb = no2, pm25_1hr_ugm3 = rep(1, length(hours))
    ),
    "Insufficient data collected for pol: no2 for year\\(s\\): 2022"
  )
  expect_setequal(as.data.frame(result$no2)$year, c(2021L, 2023L))
})

test_that("non-contiguous input dates: date order does not matter", {
  # A fully shuffled input must produce the identical result to the sorted
  # equivalent: the pipeline re-derives calendar structure from the date
  # column, not from row order. With the dense 40 ppb background all three
  # years pass the gates; the annual mean of 40 rounds to one decimal and
  # the 98th percentile of the ordered daily maxima is 40, so the 3-year
  # metric is 40 and the annual level (40 > 7.1) is Red in every window.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  set.seed(1)
  idx <- sample(seq_along(hours))

  sorted <- CAAQS_no2(
    data.frame(date = hours, no2 = 40), CAAQS_thresholds()
  )
  shuffled <- CAAQS_no2(
    data.frame(date = hours[idx], no2 = 40), CAAQS_thresholds()
  )
  expect_identical(as.data.frame(shuffled), as.data.frame(sorted))
  expect_identical(sorted$management_level_annual, c("Red", "Red", "Red"))
})

test_that("non-hourly spacing: sub-hourly sampling density is tolerated, not an error", {
  # Pinned contract (no package-wide validation policy yet, see NEWS):
  # every second hour is accepted as input. The GDAD gates then do their
  # work on the filled frame: each calendar day holds only 12 supplied
  # hours (< 18-of-24), so every day is deficient, no daily maximum is
  # valid, and the result is an empty frame -- tolerated input, correctly
  # empty output, no error.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  h2 <- hours[seq(1, length(hours), by = 2)]

  expect_no_error(
    out <- CAAQS_no2(
      data.frame(date = h2, no2 = rep(10, length(h2))), CAAQS_thresholds()
    )
  )
  expect_identical(nrow(as.data.frame(out)), 0L)
})

test_that("NA inputs: an all-NA year drops out gracefully with a warning, not an error", {
  # 2022 carries NO o3 values at all (every hour NA). The wrapper warns
  # that 2022 is insufficient and reports only 2021/2023 in the o3 frame:
  # the empty-year contract established when the NA-propagation defect was
  # fixed (no NA rows emitted, 2021/2023 metrics unaffected).
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  o3 <- rep(10, length(hours))
  o3[hours >= lubridate::ymd_h("2022-01-01 00") &
    hours < lubridate::ymd_h("2023-01-01 00")] <- NA_real_

  expect_warning(
    result <- CAAQS(
      dates = hours, o3_1hr_ppb = o3, pm25_1hr_ugm3 = rep(1, length(hours))
    ),
    "Insufficient data collected for pol: o3 for year\\(s\\): 2022"
  )
  o3_out <- as.data.frame(result$o3)
  expect_identical(o3_out$year, c(2021, 2023))
  expect_identical(
    o3_out$fourth_highest_daily_max_8hr_mean_o3, c(10, 10)
  )
})

test_that("NA inputs: an all-NA pollutant column is a clean stop, not a crash", {
  # With the only supplied pollutant entirely NA, no pollutant has three
  # consecutive complete years, so the wrapper stops with its documented
  # message. Pinned as the contract for a fully-missing pollutant feed.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  expect_error(
    CAAQS(dates = hours, pm25_1hr_ugm3 = rep(NA_real_, length(hours))),
    "Cannot calculate CAAQS without at least one pollutant with at least 3 years"
  )
})

test_that("input validation: mismatched input lengths stop with the recycling error", {
  # Pinned contract (no package-wide validation policy yet, see NEWS):
  # length mismatches surface as the underlying bind_cols recycling error,
  # which names both sizes. Exact-match on the visible message text.
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
  expect_error(
    CAAQS(dates = hours[1:100], pm25_1hr_ugm3 = rep(1, 50)),
    "Can't recycle `date` \\(size 100\\) to match `pm25` \\(size 50\\)\\.",
    fixed = FALSE
  )
})
