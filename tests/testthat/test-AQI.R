test_that("AQI returns expected output", {
  expect_snapshot(
    AQI(
      dates = lubridate::ymd_h("2024-01-01 00"),
      o3_8hr_ppm = 0.078,
      pm25_24hr_ugm3 = 35.9,
      co_8hr_ppm = 8.4
    )
  )
  expect_error(AQI())
})

# TODO: write test
test_that("AQI for PM2.5 is correct", {})

test_that("Hazardous interpolation follows the TAD's two 301-400 / 401-500 segments", {
  # EPA TAD Table 5 (2024 PM2.5 revision): 225.5-325.4 ug/m3 -> 301-400 and
  # 325.5-500.4 ug/m3 -> 401-500. The old single merged row carried the
  # 301-400 segment's concentration interval with the combined AQI range,
  # inflating in-range values (250 ug/m3 gave 350).
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 250)$AQI, 325)
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 400)$AQI, 443)
})

test_that("Beyond-the-AQI concentrations continue the final Hazardous segment", {
  # TAD FAQ: "an AQI value can still be computed ... use the same linear
  # relationship that is used for the Hazardous category." PM2.5 600 ug/m3
  # extends 325.5-500.4 -> 401-500: 556.51 -> 557 (the old code returned a
  # constant 301 for every concentration above the table).
  out <- AQI(Sys.time(), pm25_24hr_ugm3 = 600)
  expect_equal(out$AQI, 557)
  # "Beyond the AQI" is defined for AQI higher than 500 (TAD FAQ), so 500
  # itself is still Hazardous and the 501 boundary is not duplicated.
  expect_identical(as.character(out$risk_category), "Beyond the AQI")
  expect_identical(as.character(AQI_risk_category(500)), "Hazardous")
  expect_identical(as.character(AQI_risk_category(501)), "Beyond the AQI")
})

test_that("1-hour ozone extends beyond 500 per its open-ended table", {
  # 0.7 ppm extends 0.605-0.604? no: the 401-500 segment 0.505-0.604 ->
  # 0.7 ppm gives 595.85 -> 596 (old code: constant 301).
  expect_equal(AQI(Sys.time(), o3_1hr_ppm = 0.7)$AQI, 596)
})

test_that("8-hour ozone above its 0.200 ppm cap matches no row and stays NA", {
  # TAD Table 5 footnote 2: 8-hour O3 values do not define AQI >= 301; the
  # table's blank place above 0.200 ppm is disregarded. The old code
  # interpolated against Inf and surfaced -Inf with an NA category.
  expect_true(is.na(AQI(Sys.time(), o3_8hr_ppm = 0.3)$AQI))
  # The 1-hour value still counts when both are supplied (footnote 1:
  # report the higher of the two).
  expect_equal(AQI(Sys.time(), o3_8hr_ppm = 0.3, o3_1hr_ppm = 0.35)$AQI, 273)
})

test_that("the 2024 PM2.5 breakpoints classify 9.1 ug/m3 as Moderate", {
  # 2024 revision effective May 6, 2024: the Good ceiling dropped from
  # 12.0 to 9.0 ug/m3, so 9.1 is Moderate (51) and 12.0 is no longer the
  # Good/Moderate boundary.
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 9.1)$AQI, 51)
  expect_identical(
    as.character(AQI(Sys.time(), pm25_24hr_ugm3 = 9.1)$risk_category),
    "Moderate"
  )
  expect_identical(
    as.character(AQI(Sys.time(), pm25_24hr_ugm3 = 9)$risk_category),
    "Good"
  )
  expect_identical(
    as.character(AQI(Sys.time(), pm25_24hr_ugm3 = 12)$risk_category),
    "Moderate"
  )
})

test_that("SO2: the TAD's fixed-at-200 exception and the 24-hour upper end", {
  # TAD "How do I calculate AQI values for SO2?": below 305 ppb the AQI
  # uses the daily max 1-hour concentration; at or above a 305 ppb 24-hour
  # average it uses the 24-hour table; and a day whose daily max 1-hour is
  # >= 305 ppb but whose 24-hour average is not is fixed at AQI 200
  # exactly.
  d1 <- Sys.time()
  # Spike day: one 400 ppb hour among 50 ppb hours -- 24-hr average
  # 63.75 < 305, so the spike is fixed at 200 (the old code returned the
  # 1-hour table's value for the daily mean, 87).
  expect_equal(AQI(d1, so2_1hr_ppb = c(400, rep(50, 23)))$AQI, 200)
  # Sustained day: 24 x 400 ppb -- 24-hr average 400 >= 305, so the
  # 24-hour table applies (401-500 row: 232).
  expect_equal(AQI(d1, so2_1hr_ppb = rep(400, 24))$AQI, 232)
  # Interpretation choice (documented in NEWS): when no 24-hour average
  # can be derived from the supplied hours (the package derives it only
  # from >= 15 hourly values), the exception is applied conservatively --
  # a single 400 ppb hour is fixed at 200 rather than left NA.
  expect_equal(AQI(d1, so2_1hr_ppb = 400)$AQI, 200)
})

test_that("missing concentrations yield NA sub-indices, never zero", {
  # Concentrations are classified as given: an NA concentration is an NA
  # sub-index (the old code mapped NA to 0 before classification, scoring
  # missing hours as Good/0). An NA hour within a day still drops out of
  # that day's mean, so (10, NA) averages to 10 -> 53.
  expect_equal(suppressWarnings(AQI(Sys.time(), pm25_24hr_ugm3 = c(10, NA)))$AQI, 53)
})

test_that("pm25_1hr_ugm3 alone is accepted as an intermediate input", {
  # It feeds the 24-hour mean but has no breakpoint table of its own; the
  # old code crashed joining a nonexistent table.
  out <- suppressWarnings(AQI(Sys.time(), pm25_1hr_ugm3 = 10))
  expect_true(is.na(out$AQI))
})

test_that("the maximum sub-index is reported with its pollutant as principal", {
  # TAD: "the maximum of the two values reported" (ozone footnote 1; the
  # overall AQI is the maximum across pollutants).
  out <- AQI(Sys.time(), o3_8hr_ppm = 0.078, pm25_24hr_ugm3 = 35.9)
  expect_equal(out$AQI, 126)
  expect_identical(as.character(out$principal_pol), "o3")
})

# TODO: write test
test_that("AQI for PM10 is correct", {})

# TODO: write test
test_that("AQI for NO2 is correct", {})

test_that("AQI for O3 is correct", {
  output <- AQI(
    dates = c(Sys.time(), Sys.time() - lubridate::days(1)),
    o3_8hr_ppm = c(0.07853333, 0.078),
    o3_1hr_ppm = c(NA, 0.162)
  )
  # Day 1: 1-hr 0.162 ppm -> 147.487; EPA TAD 2018 step d / 2024 equation
  # post: "Round the index to the nearest integer" -> 147 (the old ceiling
  # gave 148). Day 2: 8-hr 0.078 ppm -> 125.5, half-up tie -> 126, matching
  # the TAD worked example's own value.
  expect_equal(output$AQI, c(147, 126))
})

# TODO: write test
test_that("AQI for SO2 is correct", {})

# TODO: write test
test_that("AQI for CO is correct", {})

# TODO: add more values to test
test_that("AQI for multi-pollutant is correct", {
  output <- AQI(
    dates = Sys.time(),
    o3_8hr_ppm = 0.078,
    pm25_24hr_ugm3 = 35.9,
    co_8hr_ppm = 8.4
  )
  expect_equal(output$AQI, 126)
})

# TODO: write test for concentration beyond the AQI

# TODO: test that principal pol determination is correct
