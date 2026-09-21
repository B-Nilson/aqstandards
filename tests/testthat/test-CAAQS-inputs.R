# Issue #4 matrix gaps: input handling. Three groups pin behavior the
# GDADs do not legislate (non-contiguous input dates, explicit NA inputs,
# and input validation), so the expectations are contracts established by
# probing current behavior, not GDAD citations. Where a GDAD rule *does*
# govern (the gates applied to filled rows), the test cites it.
#
# The five scenarios are owned by tests/testthat/helper-CAAQS-scenarios.R,
# shared with data-raw/CAAQS-regression-fixtures.R so the tests and the
# regenerated fixture specification cannot drift apart; this file asserts
# the behavior (including the warning and error conditions the generator
# wraps away in its document).

test_that("non-contiguous input dates: absent rows are filled and gated, present rows produce hand-checkable metrics", {
  # See helper-CAAQS-scenarios.R for the scenario arithmetic: 2022
  # supplies four isolated days, fails the NO2 GDAD Table 5-3 days
  # criteria (4/365 days, ~1% per quarter), and contributes no metric
  # rows; 2021/2023 are complete at 40 ppb.
  out <- eval(caaqs_input_scenarios$noncontiguous_sparse_days)

  expect_identical(out$year, c(2021, 2023))
  expect_identical(out$perc_98_of_daily_maxima, c(40, 40))
  expect_identical(out$annual_mean_of_hourly, c(40, 40))
})

test_that("non-contiguous input dates: date order does not matter", {
  # A fully shuffled input must produce the identical result to the
  # sorted equivalent: the pipeline re-derives calendar structure from
  # the date column, not from row order. The scenario returns TRUE only
  # when the frames are row-for-row identical AND the annual level is Red
  # in every 3-year window (40 > 7.1 ppb).
  expect_true(eval(caaqs_input_scenarios$noncontiguous_row_order))
})

test_that("non-hourly spacing: sub-hourly sampling density is tolerated, not an error", {
  # Pinned contract (no package-wide validation policy yet, see NEWS):
  # every second hour is accepted as input. The GDAD gates then do their
  # work on the filled frame: each calendar day holds only 12 supplied
  # hours (< 18-of-24), so every day is deficient and the result is an
  # empty frame -- tolerated input, correctly empty output, no error.
  expect_no_error(
    out <- eval(caaqs_input_scenarios$non_hourly_spacing)
  )
  expect_identical(nrow(as.data.frame(out)), 0L)
})

test_that("NA inputs: an all-NA year drops out gracefully with a warning, not an error", {
  # 2022 carries NO o3 values at all. The wrapper warns that 2022 is
  # insufficient and reports only 2021/2023 in the o3 frame: the
  # empty-year contract established when the NA-propagation defect was
  # fixed (no NA rows emitted, 2021/2023 metrics unaffected).
  result <- NULL
  expect_warning(
    result <- eval(caaqs_input_scenarios$all_na_year),
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
  expect_error(
    eval(caaqs_input_scenarios$all_na_column),
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
    "Can't recycle `date` \\(size 100\\) to match `pm25` \\(size 50\\)\\."
  )
})
