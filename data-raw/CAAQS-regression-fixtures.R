## Regenerate data-raw/CAAQS-regression-fixtures.md
##
## Every fixture's expected values in the generated document are COMPUTED by
## running the scenario against the package as it exists on disk (never
## hand-written), so re-running this script must reproduce the document
## byte-for-byte. Provenance per fixture: "GDAD" = the expected value follows
## from quoted guidance-document wording (cross-referenced); "PIN" = the
## value pins current behaviour that the guidance does not uniquely
## determine. Run from the package root:
##
##   Rscript data-raw/CAAQS-regression-fixtures.R
##
## No random number generation is used anywhere: every input series is a
## constant or an exact date/value list.

pkgload::load_all(quiet = TRUE, helpers = FALSE)

# Pin the print width: tibble printing adapts to it and the generated
# document must be byte-for-byte reproducible (100 keeps every selected
# column of every fixture visible).
options(width = 100L)

# Scenario helpers (make_hours, plateau) and the input-handling scenarios
# live in tests/testthat/helper-*.R, shared with the test files; source the
# single owners so the fixture inputs run against the same definitions the
# tests use.
source("tests/testthat/helper-CAAQS.R")
source("tests/testthat/helper-CAAQS-scenarios.R")

expect_columns <- function(out, cols) out[, c("year", cols)]

fixtures <- list()

# ---------------------------------------------------------------------------
fixtures$two_of_three_metric_rule <- list(
  title = "2-of-3 metric rule and incomplete-year invalidation",
  rule = paste(
    "GDAD Table 5-3 metric-value rows (all pollutants): a metric value is",
    "valid when its annual values \"are available for at least two of the",
    "possible three\" years; a year failing the annual completeness criteria",
    "contributes no annual value."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(1, length(hours))
    # 2024 loses all of Q4: 275/366 valid days = 75.1% of the year (passes
    # the year-wide 75% days gate) but Q4 has 0 valid days (< 60%), so 2024
    # fails the quarterly gate.
    no2[hours >= lubridate::ymd_h("2024-10-01 00")] <- NA
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
  }),
  note = paste(
    "A warning naming 2024 (\"Insufficient data collected for pol: no2 ...\")",
    "is part of the expected behaviour. 2024 has no row; 2023's 3-year window",
    "averages 2022 and 2023 only (2 of 3), giving 1; 2021-2022 windows are",
    "incomplete and report NA."
  ),
  columns = c("perc_98_of_daily_maxima", "3yr_mean_of_perc_98", "management_level_hourly")
)

# ---------------------------------------------------------------------------
fixtures$non_consecutive_years <- list(
  title = "Non-consecutive years do not break the 3-consecutive-years check",
  rule = paste(
    "GDAD Table 5-3 metric-value rows: three consecutive years are the",
    "averaging window; interior absent years must not poison the check. This",
    "fixture pins the bug fixed after the original audit (NA propagation",
    "through tidyr::complete()'s implicit years)."
  ),
  provenance = "PIN",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    o3 <- rep(15, length(hours))
    # 2022 is entirely absent from the data.
    gap <- hours >= lubridate::ymd_h("2022-01-01 00") &
      hours < lubridate::ymd_h("2023-01-01 00")
    hours <- hours[!gap]
    o3 <- o3[!gap]
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
  }),
  note = paste(
    "No error (before the fix: \"Cannot calculate CAAQS without at least one",
    "pollutant with at least 3 years of complete data\"). 2022 has no row;",
    "2024's 3-year window averages 2023 and 2024 (2 of 3)."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$o3_daily_18of24_gate <- list(
  title = "O3 daily gate: 18 of 24 valid rolling windows",
  rule = paste(
    "Ozone GDAD (2021) Table 5-3 (Ozone Dmax 8-hour): \"At least 18 (75%) of",
    "the 24 O3-8-hr are available in the day\"; windows need 6 of 8 hours",
    "(section 5.1) and are attributed to their ending hour (eq. 5.1)."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (day in c("04-05", "05-10", "06-15")) {
      o3 <- plateau(hours, o3, paste0("2021-", day), 62)
      o3 <- plateau(hours, o3, paste0("2022-", day), 62)
    }
    # 2021-04-20: 55 ppb over 00:00-11:00, hours 12:00-23:00 missing -> only
    # the 14 windows ending 00:00-13:00 are valid (later windows contain 3+
    # missing hours) -> 14 < 18, the day is deficient and excluded.
    o3[hours %in% make_hours("2021-04-20 00", "2021-04-20 11")] <- 55
    o3[hours %in% make_hours("2021-04-20 12", "2021-04-20 23")] <- NA
    # 2022-04-20: the same 55 ppb over all 24 hours -> 24 valid windows ->
    # ranked (4th behind the three 62 ppb days).
    o3[hours %in% make_hours("2022-04-20 00", "2022-04-20 23")] <- 55
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
  }),
  note = paste(
    "Availability alone flips the 2022 annual fourth highest from the",
    "background 10 (deficient day excluded) to 55 (identical day ranked)."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3")
)

# ---------------------------------------------------------------------------
fixtures$o3_season_restriction <- list(
  title = "O3 ranking restricted to the April 1 - September 30 season",
  rule = paste(
    "Ozone GDAD (2021) Table 5-3 (Annual fourth highest O3 Dmax 8-hour):",
    "\"O3 Dmax 8-hour are available for at least 75% of the days in the",
    "period April 1 to September 30\"; section 5.3 restricts ranking to that",
    "period."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2023) {
      for (day in c("06-10", "07-15", "08-20", "09-25")) {
        o3 <- plateau(hours, o3, paste0(year, "-", day), 50)
      }
      # Off-season plateaus (58 ppb) that must NOT enter the ranking.
      o3 <- plateau(hours, o3, paste0(year, "-03-15"), 58)
      o3 <- plateau(hours, o3, paste0(year, "-10-15"), 58)
    }
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
  }),
  note = paste(
    "The fourth highest is 50 (the four in-season elevated days), not 58:",
    "off-season daily maxima never rank."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3")
)

# ---------------------------------------------------------------------------
fixtures$o3_daily_exceedance_retention <- list(
  title = "O3 daily-row exception: deficient day retained only on exceedance",
  rule = paste(
    "Ozone GDAD (2021) Table 5-3 column 3 (Ozone Dmax 8-hour row): \"The O3",
    "Dmax 8-hour exceeds the standard\"; section 5.3 worked example: \"there",
    "are less than eighteen O3-8-hr in a given day and the O3 Dmax 8-hour",
    "based on the available data is 70 ppb. Since this O3 Dmax 8-hour",
    "exceeds the standard, it will be retained for the selection of the",
    "annual fourth highest even though the completeness criterion was not",
    "satisfied.\""
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2021-12-31 23")
    o3 <- rep(10, length(hours))
    o3 <- plateau(hours, o3, "2021-05-10", 50)
    o3 <- plateau(hours, o3, "2021-06-10", 51)
    o3 <- plateau(hours, o3, "2021-08-10", 52)
    for (day in c("2021-05-20", "2021-06-20")) {
      o3[hours %in% make_hours(paste0(day, " 00"), paste0(day, " 08"))] <- NA
      o3[hours %in% make_hours(paste0(day, " 17"), paste0(day, " 23"))] <- NA
    }
    # Deficient day EXCEEDING the 60 ppb standard in force -> retained.
    o3 <- plateau(hours, o3, "2021-05-20", 70)
    # Deficient day below the standard -> excluded.
    o3 <- plateau(hours, o3, "2021-06-20", 55)
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  }),
  note = paste(
    "Ordered daily maxima: 70 (retained), 52, 51, 50 -> fourth highest 50.",
    "Without the exception the retained day would leave only three rankable",
    "values (NA fourth highest); had the 55 ppb day also been retained the",
    "fourth highest would be 51."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$o3_annual_row_exception <- list(
  title = "O3 annual-row exception: gated-out year retained on exceedance",
  rule = paste(
    "Ozone GDAD (2021) Table 5-3 column 3 (Annual fourth highest row): \"The",
    "annual fourth highest exceeds the standard\" - a year failing the season",
    "days criterion still contributes its fourth highest when that value,",
    "computed on all available data of the year, exceeds the CAAQS in force."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2022) {
      for (day in c("06-10", "07-10")) {
        o3 <- plateau(hours, o3, paste0(year, "-", day), 70)
      }
      # Extra plateau pair per year: 2022 keeps 4 in-season days (gated in),
      # 2021 loses Aug 1 - Sep 30 -> 122/183 in-season days (66.7% < 75%)
      # -> gated out.
      for (day in c("06-20", "07-20")) {
        o3 <- plateau(hours, o3, paste0(year, "-", day), 70)
      }
    }
    o3[hours >= lubridate::ymd_h("2021-08-01 00") &
         hours < lubridate::ymd_h("2021-10-01 00")] <- NA
    # 2022 keeps 4 of 6 plateaus in season after its own gap -> 168/183
    # (91.8%) -> gated in.
    o3[hours >= lubridate::ymd_h("2022-04-01 00") &
         hours < lubridate::ymd_h("2022-05-16 00")] <- NA
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  }),
  note = paste(
    "2021 is gated out (66.7% of the season's days) but its fourth highest",
    "(70 > the 62 ppb CAAQS in force) is retained; 2022 is gated in (91.8%",
    "of the season's days). 2023's 3-year window averages 70, 70, 10 ->",
    "50. A warning naming 2021 (\"Insufficient data ...\") accompanies the",
    "CAAQS() call, not this pipeline call; the below-standard control is",
    "fixture o3_annual_exception_no_exceedance."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$o3_annual_exception_no_exceedance <- list(
  title = "O3 annual-row exception does not retain below-standard years",
  rule = paste(
    "Ozone GDAD (2021) Table 5-3 column 3 (Annual fourth highest row): the",
    "exception's condition is the exceedance itself; below-standard values",
    "of gated-out years are not retained."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2022) {
      for (day in c("06-10", "06-20", "07-10", "07-20")) {
        o3 <- plateau(hours, o3, paste0(year, "-", day), 50)
      }
    }
    o3[hours >= lubridate::ymd_h("2021-08-01 00") &
         hours < lubridate::ymd_h("2021-10-01 00")] <- NA
    o3[hours >= lubridate::ymd_h("2022-04-01 00") &
         hours < lubridate::ymd_h("2022-05-16 00")] <- NA
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  }),
  note = paste(
    "Identical gaps and day counts as o3_annual_row_exception but plateaus",
    "at 50 (< 62): 2021 drops out entirely (below-standard gated-out years",
    "are not retained), 2022 is gated in at 50, and 2023's 3-year window has",
    "only one annual value, so it reports NA."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$o3_rounding_cascade_textbox2 <- list(
  title = "O3 rounding cascade reproduces the GDAD worked example",
  rule = paste(
    "Ozone GDAD (2021) Table 5-4: O3 Dmax 8-hour reported to one decimal",
    "place; metric value (3-year average) reported as a whole number via the",
    "two-step procedure. Text Box 2: annual fourth highest of 72.5, 60.5 and",
    "55.9 ppb -> 3-year average 62.966... -> \"62.9 ppb is rounded upward 63",
    "ppb, which is the calculated ozone CAAQS metric value.\""
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2023) {
      target <- c("2021" = 72.5, "2022" = 60.5, "2023" = 55.9)[[as.character(year)]]
      for (day in c("06-10", "06-20", "07-10", "07-20")) {
        o3 <- plateau(hours, o3, paste0(year, "-", day), target)
      }
    }
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
  }),
  note = paste(
    "Four plateau days per year put each target at the annual fourth highest",
    "(the background 10 ranks fifth). The 3-year average of 72.5, 60.5,",
    "55.9 is 62.966... -> whole number 63."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$o3_attribution_cross_midnight <- list(
  title = "O3 8-hour windows attributed to their ending hour",
  rule = paste(
    "Ozone GDAD (2021) eq. 5.1: the O3-8-hr for hour J is the mean of the",
    "8-hour period ENDING at J and \"is assigned to that ending hour\". Daily",
    "maxima are taken over the 24 windows ending in each day."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    o3 <- rep(10, length(hours))
    # Three plateau nights per year: 100 ppb over 16:00-23:00 spills across",
    # midnight, so the day's maximum must come from the windows ending in",
    # the day (100) and the NEXT day keeps an 88.75 ppb window (rounds to",
    # 88.8) from the spillover.
    for (year in 2021:2023) {
      for (day in c("05-10", "06-10", "07-10")) {
        o3[hours %in% make_hours(paste0(year, "-", day, " 16"),
                                 paste0(year, "-", day, " 23"))] <- 100
      }
    }
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
  }),
  note = paste(
    "2021-2023 daily maxima include three 100-ppb days (the plateau window",
    "ending 23:00) and three 88.8 cross-midnight days (the window ending",
    "00:00 next day: (7 x 100 + 10)/8 = 88.75 -> 88.8 per Table 5-4), so the",
    "annual fourth highest is 88.8; attributing windows to their START hour",
    "instead would fold the spillover window into the plateau day and leave",
    "a fourth highest of 10. 2024 is background (fourth highest 10).",
    "3-year metric values: 2023's window (88.8 + 88.8 + 88.8)/3 = 88.8 -> 89",
    "and 2024's (88.8 + 88.8 + 10)/3 = 62.533... -> 63 (whole number,",
    "Table 5-4)."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

# ---------------------------------------------------------------------------
fixtures$no2_98p_ranking <- list(
  title = "NO2 98th percentile via the GDAD ranking approach",
  rule = paste(
    "NO2 GDAD (2020) Appendix B: the 98th percentile is the Kth highest daily",
    "maximum with K = NDM - Trunc(NDM x 0.98) (NDM = 365 -> 8th highest),",
    "ties repeated in rank order; footnote forbids interpolation (type 7)."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(1, length(hours))
    # Seven single-hour spike days per year: ordered daily maxima are
    # 100 x 7 then background 1 x 358, so the 8th highest is 1.
    for (year in 2021:2023) {
      for (d in 1:7) {
        no2[hours %in% make_hours(
          paste0(year, "-06-0", d, " 14"), paste0(year, "-06-0", d, " 14")
        )] <- 100
      }
    }
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    # Control: an 8th spike day lifts the 8th highest to 100.
    for (year in 2021:2023) {
      no2[hours %in% make_hours(
        paste0(year, "-07-02 08"), paste0(year, "-07-02 08")
      )] <- 100
    }
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    list(seven_spikes = out, eight_spikes = out2)
  }),
  note = paste(
    "seven_spikes: perc_98 = 1 (the 8th highest of 365 daily maxima, ties",
    "counted in rank order). eight_spikes: perc_98 = 100 (the eighth spike",
    "day becomes the 8th highest)."
  ),
  columns = c("perc_98_of_daily_maxima", "3yr_mean_of_perc_98", "management_level_hourly")
)

# ---------------------------------------------------------------------------
fixtures$no2_daily_exceedance_retention <- list(
  title = "NO2 daily-row exception: deficient day retained only on exceedance",
  rule = paste(
    "NO2 GDAD (2020) Table 5-3 column 3 (NO2 Dmax 1-hour row): \"The NO2",
    "Dmax 1-hour exceeds the standard\" (daily row: \"At least 18 of the 24",
    "(75%) NO2 1-hour are available in the day\")."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(1, length(hours))
    for (year in 2021:2023) {
      for (d in 1:7) {
        no2[hours %in% make_hours(
          paste0(year, "-06-0", d, " 14"), paste0(year, "-06-0", d, " 14")
        )] <- 100
      }
    }
    # 2021: deficient day (all 24 hours missing) holding 500 ppb (> the 60
    # ppb CAAQS in force) -> retained; 8 values >= 100 -> 8th highest 100.
    no2[hours %in% make_hours("2021-07-01 00", "2021-07-01 23")] <- NA
    no2[hours %in% make_hours("2021-07-01 08", "2021-07-01 08")] <- 500
    # 2022: deficient day holding 50 ppb (< the standard) -> excluded;
    # 8th highest falls back to the background 1.
    no2[hours %in% make_hours("2022-07-01 00", "2022-07-01 23")] <- NA
    no2[hours %in% make_hours("2022-07-01 08", "2022-07-01 08")] <- 50
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
  }),
  note = paste(
    "perc_98 = (100, 1, 1): the exceeding deficient day is retained, the",
    "below-standard one is not. (Retaining 2022's day would give 50.)"
  ),
  columns = c("perc_98_of_daily_maxima", "management_level_hourly")
)

# ---------------------------------------------------------------------------
fixtures$no2_annual_row_exception <- list(
  title = "NO2 annual-row exception: gated-out year's 98th percentile retained on exceedance",
  rule = paste(
    "NO2 GDAD (2020) Table 5-3 column 3 (Annual 98th percentile row): \"The",
    "98th percentile based on the available NO2 Dmax 1-hour exceeds the",
    "standard\"; the annual metric value row's exception (\"1. at least 50%",
    "of the NO2 1-hour are available in each calendar quarter; and 2. the",
    "annual average exceeds the standard\") does NOT fire here (Q4 holds no",
    "hours), so 2024's annual mean is NA even though its percentile is",
    "retained."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(5, length(hours))
    for (year in 2021:2024) {
      for (d in 1:8) {
        no2[hours %in% make_hours(
          paste0(year, "-06-0", d, " 14"), paste0(year, "-06-0", d, " 14")
        )] <- 100
      }
    }
    # 2024: Sep 15 - Dec 31 missing -> 258/366 valid days (70.5% < 75%) and
    # Q4 has no valid days/hours -> both annual gates fail. 2021-2023 stay
    # complete, satisfying CAAQS()'s 3-consecutive-complete-years check.
    no2[hours >= lubridate::ymd_h("2024-09-15 00") &
          hours < lubridate::ymd_h("2025-01-01 00")] <- NA
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
  }),
  note = paste(
    "2024 is gated out (a warning names 2024) but retained under the",
    "percentile-row exception: perc_98 = 100 > the 60 ppb CAAQS in force.",
    "annual_mean = NA for 2024 (Q4 holds no hours). The 2023 and 2024",
    "3-year percentile averages are both (100 + 100 + 100)/3 = 100."
  ),
  columns = c("perc_98_of_daily_maxima", "annual_mean_of_hourly",
              "3yr_mean_of_perc_98", "management_level_hourly")
)

# ---------------------------------------------------------------------------
fixtures$no2_50pct_quarter_exception <- list(
  title = "NO2 annual metric value exception: relaxed 50%-per-quarter path fires only on exceedance",
  rule = paste(
    "NO2 GDAD (2020) Table 5-3 column 3 (Annual metric value row): \"1. at",
    "least 50% of the NO2 1-hour are available in each calendar quarter; and",
    "2. the annual average exceeds the standard\" - the relaxed criterion",
    "replaces the 75%/60% annual gates when the annual average exceeds the",
    "CAAQS."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    # 2024: Oct 1 - Nov 15 missing -> Q4 retains exactly 50.0% of its hours
    # (1104/2208) -> 75%/60% gates fail, relaxed 50% gate holds. 2021-2023
    # stay complete for CAAQS()'s 3-consecutive-complete-years check.
    no2 <- rep(20, length(hours))
    no2[hours >= lubridate::ymd_h("2024-10-01 00") &
          hours <= lubridate::ymd_h("2024-11-15 23")] <- NA
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    # Control: background 5 ppb (< the 17 ppb annual CAAQS in force) -> the
    # exception must NOT fire and 2024 drops out.
    no2b <- rep(5, length(hours))
    no2b[hours >= lubridate::ymd_h("2024-10-01 00") &
           hours <= lubridate::ymd_h("2024-11-15 23")] <- NA
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2b)$no2
    list(exceeds = out, below = out2)
  }),
  note = paste(
    "exceeds: 2024 retained (annual_mean = 20 > 17); its 98th percentile of",
    "20 does NOT exceed the 60 ppb hourly CAAQS, so perc_98 = NA for 2024.",
    "below: 2024 has no row (years 2021-2023 only)."
  ),
  columns = c("perc_98_of_daily_maxima", "annual_mean_of_hourly",
              "management_level_hourly", "management_level_annual")
)

# ---------------------------------------------------------------------------
fixtures$no2_quarter_days_gate <- list(
  title = "NO2 annual gates: 75% of days in the year and 60% of days per quarter",
  rule = paste(
    "NO2 GDAD (2020) Table 5-3 (Annual 98th percentile row): the daily maxima",
    "\"are available for at least: 1. 75% of the days in a year; and 2. 60%",
    "of the days in each calendar quarter\"."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(1, length(hours))
    # 40 days missing from Q4 alone: Q4 keeps 52/92 days (56.5% < 60%) ->
    # 2024 fails; the year-wide criterion still holds (326/366 = 89%).
    # 2021-2023 stay complete for CAAQS()'s 3-consecutive-years check.
    no2[hours >= lubridate::ymd_h("2024-10-01 00") &
          hours < lubridate::ymd_h("2024-11-10 00")] <- NA
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    # Boundary control: 30 days missing keeps Q4 at 62/92 (67.4%) -> passes.
    no2b <- rep(1, length(hours))
    no2b[hours >= lubridate::ymd_h("2024-10-01 00") &
           hours < lubridate::ymd_h("2024-10-31 00")] <- NA
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2b)$no2
    list(fails = out, passes = out2)
  }),
  note = paste(
    "fails: 2024 has no row (a warning names 2024); 2023's 3-year window",
    "covers 2021-2023, all complete, and reports 1. passes: all four years",
    "present."
  ),
  columns = c("perc_98_of_daily_maxima", "3yr_mean_of_perc_98")
)

# ---------------------------------------------------------------------------
fixtures$so2_99p_ranking <- list(
  title = "SO2 99th percentile via the GDAD ranking approach",
  rule = paste(
    "SO2 GDAD (2020) Appendix B: the 99th percentile is the Kth highest daily",
    "maximum with K = NDM - Trunc(NDM x 0.99); with NDM = 365 the",
    "floating-point product 365 x 0.99 = 361.3499... truncates cleanly, and",
    "for NDM = 100 the product 100 x 0.99 = 98.999... (a floating-point",
    "near-miss of 99) must truncate to 99, so K = 1: the CAAQS_rank_percentile",
    "tolerance guards exactly this."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    so2 <- rep(1, length(hours))
    # Four single-hour spike days per year: ordered daily maxima are
    # 100 x 4 then background 1 x 361, so the 4th highest is 100
    # (K = 365 - Trunc(361.3499...) = 365 - 361 = 4).
    for (year in 2021:2023) {
      for (d in 1:4) {
        so2[hours %in% make_hours(
          paste0(year, "-06-0", d, " 14"), paste0(year, "-06-0", d, " 14")
        )] <- 100
      }
    }
    out <- CAAQS(dates = hours, so2_1hr_ppb = so2)$so2
    # Control: only three spike days put the 4th highest at background.
    so2b <- rep(1, length(hours))
    for (year in 2021:2023) {
      for (d in 1:3) {
        so2b[hours %in% make_hours(
          paste0(year, "-06-0", d, " 14"), paste0(year, "-06-0", d, " 14")
        )] <- 100
      }
    }
    out2 <- CAAQS(dates = hours, so2_1hr_ppb = so2b)$so2
    list(four_spikes = out, three_spikes = out2)
  }),
  note = paste(
    "four_spikes: perc_99 = 100 (the 4th highest of 365 daily maxima).",
    "three_spikes: perc_99 = 1 (the 4th highest is background).",
    "stats::quantile() (type 7) would return 1 for four_spikes, missing",
    "all four exceedance days."
  ),
  columns = c("perc_99_of_daily_maxima", "3yr_mean_of_perc_99", "management_level_annual")
)

# ---------------------------------------------------------------------------
fixtures$pm25_daily_and_annual_gates <- list(
  title = "PM2.5 daily 18-of-24 hours and annual 75%/60% gates (no exceptions)",
  rule = paste(
    "PM2.5 GDAD (2012, PN 1483) section 4.1.4: a daily 24hr-PM2.5 is valid",
    "when \"at least 75% (18 hours) of the 1-hour concentrations are available",
    "on the given day\"; annual 98P and annual average require \"at least 75%",
    "valid daily-24hr-PM2.5 in the year\" and \"at least 60% ... in each",
    "calendar quarter\" (sections 4.1.4/4.2.4). The 2012 document has no",
    "exceptions column: a gated-out year contributes nothing even when its",
    "values exceed the standard."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    pm25 <- rep(5, length(hours))
    # Seven 2021 spike days at 26 (valid daily means), plus an eighth day
    # with only 17 valid hours (2021-06-08 09:00-15:00 blanked) whose daily
    # mean must NOT rank (17 < 18).
    for (d in 1:7) {
      pm25[hours %in% make_hours(
        paste0("2021-06-0", d, " 00"), paste0("2021-06-0", d, " 23")
      )] <- 26
    }
    pm25[hours %in% make_hours("2021-06-08 00", "2021-06-08 23")] <- 26
    pm25[hours %in% make_hours("2021-06-08 09", "2021-06-08 15")] <- NA
    # 2024 loses Jul 1 - Dec 31: Q3 keeps only July's days (31/92, 33.7% <
    # 60%) -> 2024 fails. 2021-2023 stay complete for CAAQS()'s
    # 3-consecutive-years check and no later quarter is touched.
    pm25[hours >= lubridate::ymd_h("2024-07-01 00")] <- NA
    out <- CAAQS(dates = hours, pm25_1hr_ugm3 = pm25)$pm25
    # Control: the eighth day keeps all 24 hours, so it ranks too.
    pm25c <- pm25
    pm25c[hours %in% make_hours("2021-06-08 09", "2021-06-08 15")] <- 26
    out2 <- CAAQS(dates = hours, pm25_1hr_ugm3 = pm25c)$pm25
    list(deficient_day = out, complete_day = out2)
  }),
  note = paste(
    "deficient_day: 2021's perc_98 = 5 (7 valid spike days rank 1-7; the",
    "17-hour day is invalid) and 2024 has no row (a warning names 2024).",
    "complete_day: the deficient day is valid too, so 2021's ranking-approach",
    "98P of 365 daily means (365 - Trunc(0.98 x 365) = 8th highest) is 26."
  ),
  columns = c("perc_98_of_daily_means", "mean_of_daily_means",
              "3yr_mean_of_perc_98", "management_level_daily")
)

# ---------------------------------------------------------------------------
fixtures$pm25_rounding_cascade <- list(
  title = "PM2.5 one-step rounding cascade (Appendix D)",
  rule = paste(
    "PM2.5 GDAD (2012, PN 1483) Appendix D: \"numbers with second decimal",
    ".05 will be rounded upward\" (one-step half-up); daily means, annual",
    "averages and metric values reported to one decimal place (sections",
    "4.1.1, 4.2.2, 4.2.3). The daily means round BEFORE the annual average."
  ),
  provenance = "GDAD",
  input = substitute({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    pm25 <- rep(20.1, length(hours))
    pm25[hours >= lubridate::ymd_h("2023-01-01 00")] <- 20.2
    CAAQS_pm25(data.frame(date = hours, pm25 = pm25), CAAQS_thresholds())
  }),
  note = paste(
    "Annual means (20.1, 20.1, 20.2); 3-year average (20.1 + 20.1 + 20.2)/3",
    "= 20.133... -> second decimal 3 (< .05) -> 20.1."
  ),
  columns = c("mean_of_daily_means", "3yr_mean_of_means")
)

# ---------------------------------------------------------------------------
fixtures$band_edge_classification <- list(
  title = "Management-level band edges (Air Zone Management GDAD Appendix 2)",
  rule = paste(
    "Guidance Document on Air Zone Management (2019) Appendix 2, Table A2-1:",
    "O3 2020 levels \"Red > 62 ppb; Orange 57 to 62 ppb; Yellow 51 to 56",
    "ppb; Green < 50 ppb\". The achievement GDADs state the metric \"is",
    "achieved if ... less than or equal to the standard\", so Red is strict",
    "> and the Orange/Yellow lower edges are inclusive."
  ),
  provenance = "GDAD",
  input = substitute({
    th <- CAAQS_thresholds()$o3$`8hr`
    values <- c(62, 62.00001, 57, 56.9, 51, 50.9)
    out <- vapply(
      values,
      function(v) CAAQS_meets_standard(2022, v, th),
      character(1)
    )
    setNames(unname(out), values)
  }),
  note = paste(
    "62 -> Orange; 62.00001 -> Red; 57 -> Orange; 56.9 -> Yellow;",
    "51 -> Yellow; 50.9 -> Green. (2022 thresholds: Red 62, Orange 57,",
    "Yellow 51.)"
  ),
  columns = NULL
)

# ---------------------------------------------------------------------------
# Input-handling fixtures (issue #4 matrix: non-contiguous dates, explicit
# NA inputs, input validation). The GDADs do not legislate input handling,
# so these expectations are behavior contracts pinned by probing; where a
# GDAD rule does govern (the gates applied to filled rows) the citation is
# given.
# ---------------------------------------------------------------------------
fixtures$noncontiguous_sparse_days <- list(
  title = "Non-contiguous input dates: absent rows are filled, then gated",
  rule = paste(
    "No GDAD rule forbids sparse input: absent dates are treated as missing",
    "hours. The NO2 GDAD (2020) Table 5-3 days criteria then gate the result:",
    "2022 supplies only four isolated days (2022-01-15, 2022-03-03,",
    "2022-07-09, 2022-11-28) at 40 ppb, so 4/365 days (1.1%) < 75% of days",
    "and each quarter holds 1 day (~1%) < 60%: 2022 contributes no metric",
    "rows. 2021/2023 are complete at 40 ppb."
  ),
  provenance = "GDAD",
  input = caaqs_input_scenarios$noncontiguous_sparse_days,
  note = paste(
    "Rows only for 2021 and 2023, each perc_98 = 40 (the ordered daily",
    "maxima are all 40). The all-NA 2022 hours are filled rows, not input",
    "errors."
  ),
  columns = "perc_98_of_daily_maxima"
)

fixtures$noncontiguous_row_order <- list(
  title = "Non-contiguous input dates: row order does not matter",
  rule = paste(
    "The pipeline re-derives calendar structure from the date column, not",
    "from row order: a fully shuffled input produces the identical result",
    "to the sorted equivalent. Pinned contract (no package-wide validation",
    "policy yet); with the dense 40 ppb background all three years pass the",
    "gates, and the annual level (40 > 7.1 ppb) is Red in every window."
  ),
  provenance = "PIN",
  input = caaqs_input_scenarios$noncontiguous_row_order,
  note = "TRUE: shuffled and sorted inputs agree row-for-row and level-for-level.",
  columns = NULL
)

fixtures$non_hourly_spacing <- list(
  title = "Sub-hourly sampling density is tolerated, not an error",
  rule = paste(
    "Pinned contract (no package-wide validation policy yet): every second",
    "hour is accepted as input. The NO2 GDAD Table 5-3 daily criterion then",
    "does the work: each calendar day holds only 12 supplied hours",
    "(< 18-of-24), so every day is deficient and the result is an empty",
    "frame - tolerated input, correctly empty output."
  ),
  provenance = "GDAD",
  input = caaqs_input_scenarios$non_hourly_spacing,
  note = "A zero-row frame: no day reaches the 18-of-24 valid-hours criterion.",
  columns = "perc_98_of_daily_maxima"
)

fixtures$all_na_year <- list(
  title = "An all-NA year drops out gracefully with a warning",
  rule = paste(
    "2022 carries no o3 values at all. The wrapper warns that 2022 is",
    "insufficient and reports only 2021/2023 in the o3 frame: the empty-year",
    "contract established with the NA-propagation fix (no NA rows emitted,",
    "neighbouring years unaffected)."
  ),
  provenance = "PIN",
  # The warning itself ("Insufficient data collected for pol: o3 for
  # year(s): 2022 ...") is asserted in test-CAAQS-inputs.R; the document
  # wrapper silences it for deterministic output.
  input = substitute(
    suppressWarnings(EXPR)$o3,
    list(EXPR = caaqs_input_scenarios$all_na_year)
  ),
  note = paste(
    "Rows only for 2021/2023, each fourth-highest 10 and 3-year mean NA",
    "(two years in the 2021-2023 window)."
  ),
  columns = c("fourth_highest_daily_max_8hr_mean_o3", "3yr_mean")
)

fixtures$all_na_column <- list(
  title = "An all-NA pollutant column is a clean stop, not a crash",
  rule = paste(
    "With the only supplied pollutant entirely NA, no pollutant has three",
    "consecutive complete years, so the wrapper stops with its documented",
    "message. Pinned as the contract for a fully-missing pollutant feed."
  ),
  provenance = "PIN",
  input = substitute(
    tryCatch(EXPR, error = function(e) conditionMessage(e)),
    list(EXPR = caaqs_input_scenarios$all_na_column)
  ),
  note = paste(
    "The message string: Cannot calculate CAAQS without at least one",
    "pollutant with at least 3 years of complete data."
  ),
  columns = NULL
)

# ---------------------------------------------------------------------------
## Run every fixture and capture its exact output
results <- list()
for (nm in names(fixtures)) {
  fx <- fixtures[[nm]]
  value <- eval(fx$input)
  # Extract the data.frame-ish result (fixtures may return a list of them).
  frames <- if (is.list(value) && !is.data.frame(value)) value else list(value)
  results[[nm]] <- lapply(frames, function(f) {
    if (is.data.frame(f)) expect_columns(f, intersect(fx$columns, names(f))) else f
  })
  # Single-frame fixtures come out unnamed; give them a stable label so the
  # emit loop below prints them.
  if (is.null(names(results[[nm]]))) names(results[[nm]]) <- "result"
  fx$input_text <- deparse(fx$input) |>
    paste(collapse = "\n") |>
    sub("^\\{\n  ", "", x = _) |>
    sub("\n\\}$", "", x = _)
  fixtures[[nm]] <- fx
}

## Emit the markdown specification
lines <- c(
  "# CAAQS regression-test fixture specification",
  "",
  "Deterministic fixtures for the follow-up regression-test issue. Each",
  "fixture pins one implemented rule of `CAAQS()` with a minimal synthetic",
  "input (exact values and dates, no random data) and the exact expected",
  "output **computed by running the scenario against the package as",
  "committed** - regenerate with `Rscript data-raw/CAAQS-regression-fixtures.R`",
  "and the document must be reproduced byte-for-byte.",
  "",
  "Helpers: the input blocks call two scenario helpers, owned by",
  "`tests/testthat/helper-CAAQS.R`, and the input-handling scenarios,
  owned by `tests/testthat/helper-CAAQS-scenarios.R`, all shared with the",
  "package's tests; their definitions are embedded verbatim below so every",
  "fixture is runnable as written:",
  ""
)
lines <- c(lines, "```r", readLines("tests/testthat/helper-CAAQS.R"), "```", "")
lines <- c(lines, "```r", readLines("tests/testthat/helper-CAAQS-scenarios.R"), "```", "")
lines <- c(
  lines,
  "Coverage caveats: every completeness and exceptions criterion of the",
  "four guidance documents has at least one fixture except three SO2 cases",
  "(daily-row exceedance retention, the annual-row percentile exception,",
  "and the annual-metric-value relaxed path), omitted as NO2 twins - they",
  "exercise the identical shared code paths and differ only in the",
  "pollutant name and percentile; see fixtures no2_daily_exceedance_retention,",
  "no2_annual_row_exception and no2_50pct_quarter_exception.",
  "",
  "Input handling (issue #4): the GDADs legislate no input validation, so",
  "non-contiguous dates, sub-hourly spacing, and NA inputs are pinned as",
  "behaviour contracts (see fixtures noncontiguous_*, non_hourly_spacing,",
  "all_na_*).",
  "",
  "Provenance legend: **GDAD** = the expectation follows from quoted",
  "guidance-document wording (cited per fixture); **PIN** = the expectation",
  "pins current behaviour that the guidance does not uniquely determine.",
  "Guidance texts: Ozone GDAD (2021), NO2 GDAD (2020), SO2 GDAD (2020),",
  "PM2.5 GDAD (2012, PN 1483) - CCME Guidance Documents on Achievement",
  "Determination - and the CCME Guidance Document on Air Zone Management",
  "(2019).",
  "",
  "Fixture index:",
  "",
  paste0(
    sprintf("%d. `%s` - %s", seq_along(fixtures), names(fixtures),
            vapply(fixtures, function(f) f$title, "")),
    collapse = "\n"
  ),
  ""
)

for (nm in names(fixtures)) {
  fx <- fixtures[[nm]]
  lines <- c(lines, c(
    paste0("## ", nm),
    "",
    paste0("**", fx$title, "** (", fx$provenance, ")"),
    "",
    paste0("Rule: ", fx$rule),
    "",
    "Input:",
    "",
    "```r",
    fx$input_text,
    "```",
    "",
    paste0("Expected: ", fx$note),
    "",
    "Exact expected output (computed):",
    "",
    "```r"
  ))
  for (frame_nm in names(results[[nm]])) {
    frame <- results[[nm]][[frame_nm]]
    lines <- c(lines, paste0("# ", frame_nm))
    lines <- c(lines, utils::capture.output(print(frame)))
  }
  lines <- c(lines, c("```", ""))
}

writeLines(lines, "data-raw/CAAQS-regression-fixtures.md")
cat("Wrote data-raw/CAAQS-regression-fixtures.md (", length(lines), "lines )\n")
