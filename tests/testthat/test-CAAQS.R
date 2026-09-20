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

test_that("O3 daily max uses all 8-hour rolling windows, not non-overlapping blocks", {
  # Five years of hourly data at a 10 ppb background, with four elevated days
  # per year holding an exact 8-hour plateau (09:00-16:00) so that one rolling
  # window matches the plateau exactly. The daily maxima are therefore
  # 58, 59, 61, 62 ppb per year and the annual 4th-highest is 58 ppb.
  # Non-overlapping floor_date("8 hours") blocks straddle the plateaus and
  # understate the maxima (largest block mean is 54.75 ppb), so the old
  # implementation returned a 4th-highest below 58 and a lower management
  # level. The first two 3-year windows are incomplete, so no metric is
  # reported for 2021/2022.
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
  expect_identical(output$`3yr_mean`, c(NA_real_, NA_real_, 58, 58, 58))
  # 58 ppb exceeds the Orange bound (56.01) but not the Red CAAQS (62/60).
  expect_identical(
    output$management_level_8hr,
    c(NA, NA, "Orange", "Orange", "Orange")
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
