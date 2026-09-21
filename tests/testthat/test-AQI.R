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

# Boundary-sweep machinery. Expected values are derived from the EPA TAD
# equation applied to the package's own breakpoint rows (AQI = aqi_low +
# (aqi_high - aqi_low) / (bp_high - bp_low) * (conc - bp_low), rounded
# half-up per TAD step (d)), so every row boundary is exercised without
# hand-transcribing all ~60 of them; per-pollutant hand anchors below pin
# the equation itself against TAD-worked arithmetic, so a corrupted table
# or a broken equation cannot satisfy both. Dates: mid-June, deliberately
# away from DST transitions -- AQI() works in naive local time.
AQI_sweep_expectations <- function(pol) {
  bps <- getFromNamespace("AQI_breakpoints", "aqstandards")[[pol]]
  is_open <- is.infinite(bps$bp_high)
  last <- max(which(!is_open))
  # Beyond extension follows the last closed segment's line (TAD FAQ)
  ext_bp_high <- bps$bp_low[is_open] + (bps$bp_high[last] - bps$bp_low[last])
  bps$bp_high[is_open] <- ext_bp_high
  bps$aqi_high[is_open] <-
    bps$aqi_low[last] + (bps$aqi_high[last] - bps$aqi_low[last]) *
      (ext_bp_high - bps$bp_low[last]) / (bps$bp_high[last] - bps$bp_low[last])
  # Anchor the line at the last closed segment's low end (the TAD's "same
  # linear relationship" is that segment's line), so expected values
  # reproduce the code's continuous extension.
  bps$bp_low[is_open] <- bps$bp_low[last]
  bps$aqi_low[is_open] <- bps$aqi_low[last]
  bps
}
AQI_row_of <- function(pol, conc) {
  bps <- getFromNamespace("AQI_breakpoints", "aqstandards")[[pol]]
  which(conc >= bps$bp_low & conc <= bps$bp_high)
}
AQI_expected_from_row <- function(conc, row) {
  raw <- row$aqi_low +
    (row$aqi_high - row$aqi_low) / (row$bp_high - row$bp_low) *
      (conc - row$bp_low)
  floor(raw + 0.5)
}
AQI_sweep <- function(pol, conc, row_no) {
  bps <- AQI_sweep_expectations(pol)
  # A 24-hour-replicated input exercises the daily aggregation too; the
  # 1-hour-replicated input keeps SO2's 1-hr/24-hr split unambiguous.
  rep24 <- rep(conc, 24)
  out <- do.call(
    AQI,
    c(
      list(dates = seq(lubridate::ymd_h("2026-06-01 00"), by = "hour", length.out = 24)),
      stats::setNames(list(rep24), pol)
    )
  )
  stopifnot(length(out$AQI) == 1)
  expect_equal(
    out$AQI,
    AQI_expected_from_row(conc, bps[row_no, ]),
    info = sprintf("%s conc=%s row=%d", pol, format(conc), row_no)
  )
}

test_that("AQI for PM2.5 is correct", {
  # 2024 revision rows (effective May 6, 2024; TAD Table 5 as revised):
  # every row boundary from both sides plus one interior point, expected
  # from the TAD equation on the package's rows; hand anchors below pin
  # the equation to TAD-worked arithmetic.
  pol <- "pm25_24hr_ugm3"
  bps <- AQI_sweep_expectations("pm25_24hr_ugm3")
  edges <- c(0, 9, 9.1, 35.4, 35.5, 55.4, 55.5, 125.4, 125.5, 225.4, 225.5, 325.4, 325.5, 500.4, 500.5)
  for (i in seq_along(edges)) {
    conc <- edges[[i]]
    row_no <- AQI_row_of(pol, conc)
    AQI_sweep("pm25_24hr_ugm3", conc, row_no)
  }
  # Hand anchors (TAD arithmetic): 5 ug/m3 -> 50/9*5 = 27.8 -> 28;
  # 45.7 ug/m3 -> 101 + 49/19.9*(45.7-35.5) = 126.1 -> 126.
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 5)$AQI, 28)
  expect_equal(AQI(Sys.time(), pm25_24hr_ugm3 = 45.7)$AQI, 126)
  # Missing inputs: NA/NaN trip the explicit all-missing guard; Inf finds
  # no row (beyond even the Beyond extension) and yields NA -- pinned as
  # the current contract.
  expect_error(AQI(Sys.time(), pm25_24hr_ugm3 = NA),
               "at least 1 non-NA value")
  expect_error(AQI(Sys.time(), pm25_24hr_ugm3 = NaN),
               "at least 1 non-NA value")
  expect_true(is.na(AQI(Sys.time(), pm25_24hr_ugm3 = Inf)$AQI))
})


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

test_that("AQI for PM10 is correct", {
  # TAD Table 5 PM10 rows: both sides of every boundary plus interior
  # points; hand anchors pin the equation.
  pol <- "pm10_24hr_ugm3"
  bps <- AQI_sweep_expectations("pm10_24hr_ugm3")
  edges <- c(0, 54, 55, 154, 155, 254, 255, 354, 355, 424, 425, 504, 505, 604, 605)
  for (i in seq_along(edges)) {
    conc <- edges[[i]]
    row_no <- AQI_row_of(pol, conc)
    AQI_sweep("pm10_24hr_ugm3", conc, row_no)
  }
  # Beyond the AQI continues the final segment's line: 605-700 -> 501.
  AQI_sweep("pm10_24hr_ugm3", 700, 8)
  # Hand anchors: 30 ug/m3 -> 50/54*30 = 27.8 -> 28; 300 ug/m3 ->
  # 151 + 49/99*(300-255) = 173.3 -> 173.
  expect_equal(AQI(Sys.time(), pm10_24hr_ugm3 = 30)$AQI, 28)
  expect_equal(AQI(Sys.time(), pm10_24hr_ugm3 = 300)$AQI, 173)
})


test_that("AQI for NO2 is correct", {
  # TAD Table 5 NO2 1-hour rows; the daily max of the replicated hours
  # is the concentration itself.
  pol <- "no2_1hr_ppb"
  bps <- AQI_sweep_expectations("no2_1hr_ppb")
  edges <- c(0, 53, 54, 100, 101, 360, 361, 649, 650, 1249, 1250, 1649, 1650, 2049, 2050)
  for (i in seq_along(edges)) {
    conc <- edges[[i]]
    row_no <- AQI_row_of(pol, conc)
    AQI_sweep("no2_1hr_ppb", conc, row_no)
  }
  # Beyond the AQI: 2050-4050 continues the final segment's line.
  AQI_sweep("no2_1hr_ppb", 3000, 8)
  # Hand anchors: 40 ppb -> 50/53*40 = 37.7 -> 38; 500 ppb ->
  # 151 + 49/288*(500-361) = 174.7 -> 175.
  expect_equal(AQI(Sys.time(), no2_1hr_ppb = 40)$AQI, 38)
  expect_equal(AQI(Sys.time(), no2_1hr_ppb = 500)$AQI, 175)
  # Truncation to 0 digits (TAD step (a)): 53.7 ppb truncates to 53 -> 50.
  expect_equal(AQI(Sys.time(), no2_1hr_ppb = 53.7)$AQI, 50)
})


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

test_that("AQI for SO2 is correct", {
  # TAD Table 5 SO2: 1-hour rows define up to AQI 200; 24-hour rows
  # define 201-500 and Beyond (TAD "How do I calculate AQI values for
  # SO2?"). 1-hour sweeps use the daily max of replicated hours; 24-hour
  # sweeps feed 24 hours so the derived daily average is the value.
  pol <- "so2_1hr_ppb"
  bps <- AQI_sweep_expectations("so2_1hr_ppb")
  edges <- c(0, 35, 36, 75, 76, 185, 186, 304)
  for (i in seq_along(edges)) {
    conc <- edges[[i]]
    row_no <- AQI_row_of(pol, conc)
    AQI_sweep("so2_1hr_ppb", conc, row_no)
  }
  bps24 <- AQI_sweep_expectations("so2_24hr_ppb")
  edges24 <- c(305, 604, 605, 804, 805, 1004, 1005)
  for (i in seq_along(edges24)) {
    conc <- edges24[[i]]
    row_no <- AQI_row_of("so2_24hr_ppb", conc)
    # 24-hour-basis input: so2_24hr_ppb is derived from >= 15 hourly
    # values, so supply a full day.
    out <- AQI(seq(lubridate::ymd_h("2026-06-01 00"), by = "hour", length.out = 24),
               so2_1hr_ppb = rep(conc, 24))
    stopifnot(length(out$AQI) == 1)
    expect_equal(
      out$AQI,
      AQI_expected_from_row(conc, bps24[row_no, ]),
      info = sprintf("so2_24hr conc=%s row=%d", format(conc), row_no)
    )
  }
  # Beyond the AQI for the 24-hour basis continues the final line.
  out <- AQI(seq(lubridate::ymd_h("2026-06-01 00"), by = "hour", length.out = 24),
             so2_1hr_ppb = rep(1200, 24))
  expect_equal(out$AQI, AQI_expected_from_row(1200, bps24[4, ]))
  # Hand anchors: 20 ppb -> 50/35*20 = 28.6 -> 29; a full day of 400 ppb
  # averages 400 -> 24-hour row 305-604: 201 + 99/299*95 = 232.4 -> 232.
  expect_equal(AQI(Sys.time(), so2_1hr_ppb = 20)$AQI, 29)
  expect_equal(AQI(Sys.time(), so2_1hr_ppb = rep(400, 24))$AQI, 232)
})


test_that("AQI for CO is correct", {
  # TAD Table 5 CO 8-hour rows; the daily max of replicated hours is the
  # concentration itself.
  pol <- "co_8hr_ppm"
  bps <- AQI_sweep_expectations("co_8hr_ppm")
  edges <- c(0, 4.4, 4.5, 9.4, 9.5, 12.4, 12.5, 15.4, 15.5, 30.4, 30.5, 40.4, 40.5, 50.4, 50.5)
  for (i in seq_along(edges)) {
    conc <- edges[[i]]
    row_no <- AQI_row_of(pol, conc)
    AQI_sweep("co_8hr_ppm", conc, row_no)
  }
  # Beyond the AQI continues the final segment's line.
  AQI_sweep("co_8hr_ppm", 60, 8)
  # Hand anchors: 2 ppm -> 50/4.4*2 = 22.7 -> 23; 20 ppm ->
  # 201 + 99/14.9*(20-15.5) = 230.9 -> 231.
  expect_equal(AQI(Sys.time(), co_8hr_ppm = 2)$AQI, 23)
  expect_equal(AQI(Sys.time(), co_8hr_ppm = 20)$AQI, 231)
})


test_that("AQI for multi-pollutant is correct", {
  output <- AQI(
    dates = Sys.time(),
    o3_8hr_ppm = 0.078,
    pm25_24hr_ugm3 = 35.9,
    co_8hr_ppm = 8.4
  )
  expect_equal(output$AQI, 126)
})

test_that("principal pollutant selection and ties are deterministic", {
  # The overall AQI is the maximum sub-index (TAD) and the risk category
  # always matches the final AQI value.
  out <- AQI(Sys.time(), o3_8hr_ppm = 0.078, pm25_24hr_ugm3 = 35.9, co_8hr_ppm = 8.4)
  expect_equal(out$AQI, 126)
  expect_identical(as.character(out$risk_category), "Unhealthy for Sensitive Groups")
  expect_identical(as.character(out$principal_pol), "o3")
  # Ties resolve to the first of the tied sub-index columns in the daily
  # aggregation's output order (pm25, pm10, so2_24hr, o3_8hr, o3_1hr,
  # co_8hr, co_1hr, so2_1hr, no2_1hr -- which.max takes the first
  # maximum). The TAD does not define tie-breaking; this documents and
  # pins the implementation's choice.
  expect_identical(
    as.character(AQI(Sys.time(), o3_8hr_ppm = 0.078, pm25_24hr_ugm3 = 45.7)$principal_pol),
    "pm25"
  )
  expect_identical(
    as.character(AQI(Sys.time(), co_8hr_ppm = 9.4, so2_1hr_ppb = rep(75, 24))$principal_pol),
    "co"
  )
  # A day missing one pollutant still uses the other; each row's
  # principal matches the pollutant that produced its AQI.
  partial <- AQI(
    lubridate::ymd_h("2026-06-01 00") + lubridate::hours(c(0, 24)),
    pm25_24hr_ugm3 = c(40, NA),
    o3_8hr_ppm = c(NA, 0.080)
  )
  expect_equal(partial$AQI, c(112, 133))
  expect_identical(as.character(partial$principal_pol), c("pm25", "o3"))
})

test_that("the 15-valid-hour gate and date-gap filling behave as documented", {
  # The TAD states no numeric completeness requirement for a valid day
  # (unlike the CAAQS GDADs); the >= 15 of 24 hourly values needed to
  # derive a 24-hour mean is the package's pre-existing tolerance,
  # documented as an interpretation choice. 14 valid hours -> no derived
  # mean -> NA; 15 -> 40 ug/m3 -> 111.
  h24 <- seq(lubridate::ymd_h("2026-06-01 00"), by = "hour", length.out = 24)
  expect_true(is.na(
    suppressWarnings(AQI(h24, pm25_1hr_ugm3 = c(rep(40, 14), rep(NA, 10)))$AQI)
  ))
  expect_equal(
    suppressWarnings(AQI(h24, pm25_1hr_ugm3 = c(rep(40, 15), rep(NA, 9)))$AQI),
    112
  )
  # Gaps between supplied dates are filled with NA rows without changing
  # chronology: day 1 and day 3 keep their values, the absent day is NA.
  gapped <- suppressWarnings(AQI(
    c(h24[1:4], h24[1:4] + lubridate::days(2)),
    pm25_24hr_ugm3 = c(rep(40, 4), rep(10, 4))
  ))
  expect_equal(nrow(gapped), 3)
  expect_equal(gapped$AQI, c(112, NA, 53))
  expect_true(all(diff(as.numeric(gapped$date)) > 0))
})
