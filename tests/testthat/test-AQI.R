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

test_that("truncation applies to every pollutant before classification", {
  # TAD step (a): "Truncate ... to the number of decimal places shown in
  # the breakpoint table" -- PM2.5/CO 1, PM10/SO2/NO2 0, O3 3 digits.
  # The old code passed a single piped string to starts_with(), which
  # matches nothing, silently disabling truncation for every pollutant
  # except O3: sub-boundary concentrations fell into the tables' gaps and
  # returned NA instead of the truncated row's value.
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 9.05)$AQI, 50)
  expect_equal(AQI(Sys.time(), so2_1hr_ppb = 35.7)$AQI, 50)
  expect_equal(AQI(Sys.time(), pm10_24hr_ugm3 = 54.5)$AQI, 50)
  expect_equal(AQI(Sys.time(), co_8hr_ppm = 4.45)$AQI, 50)
  # Truncation must not eat a boundary: 9.1 truncates to itself and is
  # Moderate, 9.05 truncates to 9.0 and stays Good.
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 9.1)$AQI, 51)
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 35.5)$AQI, 101)
})

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
  # continues the 325.5-500.4 -> 401-500 segment LINE from its low anchor:
  # 401 + (600-325.5)/174.9*99 = 556.4 -> 556 (the old code returned a
  # constant 301 for every concentration above the table).
  # Continuity: the extension is anchored at the segment low end, so 500.5
  # ug/m3 interpolates to 500 -- no +1 step at the 500 boundary. The AQI
  # is unbounded: 700 ug/m3 -> 401 + (700-325.5)/174.9*99 = 613.
  out <- AQI(Sys.time(), pm25_24hr_ugm3 = 600)
  expect_equal(out$AQI, 556)
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

test_that("derived 8-hour windows are attributed by start hour, per the TAD", {
  # TAD FAQ: the daily maximum 8-hour average runs over "the 17 consecutive
  # moving 8-hour periods in each day, beginning with the 8-hour period
  # from 7am to 3pm" -- windows are identified by their start hour, changed
  # with the 2015 ozone standard "to avoid double-counting an exceedance
  # from a single, short-term episode that spans the nighttime hours of
  # the first day into the early hours of the second day". The old code
  # kept end-attributed window values, re-attributing a midnight-spanning
  # episode to the day after it began.
  d <- lubridate::ymd_h("2026-06-01 00") + lubridate::hours(0:47)
  # Plateau 20:00 day 1 - 03:00 day 2: only day 1 has start-hours (20-23)
  # inside the plateau, so the episode belongs to day 1 (133 = 0.080 ppm);
  # day 2's start-hour windows are all 0.030 (28 = Good).
  plateau <- c(rep(0.030, 20), rep(0.080, 8), rep(0.030, 20))
  expect_equal(AQI(dates = d, o3_1hr_ppm = plateau)$AQI, c(133, 28))
  # Control: the same plateau shifted to 08:00-15:00 stays within one day.
  midday <- c(rep(0.030, 8), rep(0.080, 8), rep(0.030, 32))
  expect_equal(AQI(dates = d, o3_1hr_ppm = midday)$AQI, c(133, 28))
})

test_that("hourly-basis pollutants aggregate by daily maximum, per the TAD", {
  # TAD: an AQI value needs "the max 1-hour or 8-hour value in a 24-hour
  # period" for non-PM pollutants. The old code averaged the hourly
  # series, hiding intra-day peaks: 80 ppb NO2 for one hour among 10s
  # averaged to 14.2 ppb (AQI 13) instead of using the daily max.
  d <- lubridate::ymd_h("2026-06-01 00") + lubridate::hours(0:47)
  no2 <- c(20, 80, 30, rep(10, 21), rep(10, 24))
  out <- AQI(dates = d, no2_1hr_ppb = no2)
  # Day 1 max 80 ppb: 51 + 49/46 * (80 - 54) = 78.7 -> 79 (Moderate);
  # day 2 max 10 ppb: 50/53 * 10 = 9.4 -> 9 (Good).
  expect_equal(out$AQI, c(79, 9))
})

test_that("24-hour-basis pollutants aggregate by daily mean, per the TAD", {
  # TAD: "24 hourly values for PM" -- PM2.5/PM10 (and the supplied SO2
  # 24-hour average) remain daily averages, unlike the hourly bases.
  d <- lubridate::ymd_h("2026-06-01 00") + lubridate::hours(0:47)
  pm25 <- c(rep(60, 12), rep(20, 12), rep(20, 12), rep(10, 12))
  out <- AQI(dates = d, pm25_24hr_ugm3 = pm25)
  # Day 1 mean 40.0: 101 + 49/19.9 * (40 - 35.5) = 112.1 -> 112 (2024
  # table, Unhealthy for Sensitive Groups); day 2 mean 15.0:
  # 51 + 49/26.3 * (15 - 9.1) = 62.0 -> 62 (Moderate).
  expect_equal(out$AQI, c(112, 62))
})

test_that("ozone daily maxima capture night-time 8-hour peaks", {
  # TAD FAQ: the daily maximum 8-hour average is the max over the 17
  # windows beginning 7 am -- an overnight plateau belongs to the day it
  # peaks in. A 0.075 ppm small-hours peak must not be averaged away
  # across the day (old behaviour: 0.0488 daily mean -> AQI 44).
  d <- lubridate::ymd_h("2026-06-01 00") + lubridate::hours(0:47)
  o3 <- c(rep(0.075, 6), rep(0.040, 18), rep(0.040, 24))
  out <- AQI(dates = d, o3_8hr_ppm = o3)
  # Day 1 max 0.075 ppm: 101 + 49/0.014 * (0.075 - 0.071) = 115;
  # day 2 max 0.040 ppm: 50/0.054 * 0.040 = 37.
  expect_equal(out$AQI, c(115, 37))
})

test_that("hourly PM2.5 input truncates after daily aggregation", {
  # TAD step a truncates the daily concentration entering the table.
  # 23 hours of 4 plus one hour of 60 ug/m3: the 24-hour mean is 6.333,
  # truncated to 6.3 (2024 Good band 0-9.0: 50/9 * 6.3 = 35).
  d <- lubridate::ymd_h("2026-06-01 00") + lubridate::hours(0:23)
  expect_equal(
    suppressWarnings(AQI(dates = d, pm25_1hr_ugm3 = c(rep(4, 23), 60)))$AQI,
    35
  )
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
