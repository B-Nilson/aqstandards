# CAAQS regression-test fixture specification

Deterministic fixtures for the follow-up regression-test issue. Each
fixture pins one implemented rule of `CAAQS()` with a minimal synthetic
input (exact values and dates, no random data) and the exact expected
output **computed by running the scenario against the package as
committed** - regenerate with `Rscript data-raw/CAAQS-regression-fixtures.R`
and the document must be reproduced byte-for-byte.

Helpers: the input blocks call two scenario helpers, owned by
`tests/testthat/helper-CAAQS.R`, and the input-handling scenarios,
  owned by `tests/testthat/helper-CAAQS-scenarios.R`, all shared with the
package's tests; their definitions are embedded verbatim below so every
fixture is runnable as written:

```r
# Shared scenario helpers for CAAQS tests and the regression-fixture
# generator (data-raw/CAAQS-regression-fixtures.R sources this file so the
# fixture inputs run against the same helpers the tests use). testthat
# auto-sources every helper-*.R before the test files.

# Hourly timestamps from `start` to `end`, inclusive, one hour apart;
# arguments are the "YYYY-MM-DD HH" strings lubridate::ymd_h() parses.
make_hours <- function(start, end) {
  seq(lubridate::ymd_h(start), lubridate::ymd_h(end), "1 hours")
}

# Set the hours of `day` from 09:00 to 16:00 (8 consecutive hourly
# timestamps) to `value`.
plateau <- function(hours, values, day, value, from = "09", to = "16") {
  values[hours %in% make_hours(paste(day, from), paste(day, to))] <- value
  values
}
```

```r
# Single owner of the five input-handling scenarios (issue #4 matrix gaps:
# non-contiguous input dates, explicit NA inputs, input validation). Both
# test-CAAQS-inputs.R and data-raw/CAAQS-regression-fixtures.R consume these
# definitions, so a scenario edited here changes the tests, the generator,
# and the regenerated specification document together -- editing one side
# without the other fails (the anti-rot test locks the committed .md to a
# fresh regeneration).
#
# The GDADs legislate no input handling, so these are behaviour contracts
# pinned by probing current code, not GDAD citations; where a GDAD rule
# does govern (the gates applied to filled rows) the consuming files cite
# it. The expressions are condition-transparent on purpose: the tests
# assert the warning/error, and the generator adds its own
# suppressWarnings()/tryCatch() wrappers for the document.

caaqs_input_scenarios <- list(
  # 2022 supplies only four isolated days (2022-01-15, 2022-03-03,
  # 2022-07-09, 2022-11-28) of hourly NO2 = 40 ppb; 2021 and 2023 are
  # complete at the same value. Absent dates are not errors: the pipeline
  # fills them as missing. The NO2 GDAD Table 5-3 gates then do the work:
  # 4/365 days = 1.1% of days, and every quarter holds exactly 1 day
  # (1/90 or 1/92 = ~1%), so 2022 fails the 75%-of-days and 60%-per-quarter
  # days criteria and contributes no metric rows.
  noncontiguous_sparse_days = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(40, length(hours))
    no2[hours >= lubridate::ymd_h("2022-01-01 00") &
      hours < lubridate::ymd_h("2023-01-01 00")] <- NA_real_
    for (day in c("2022-01-15", "2022-03-03", "2022-07-09", "2022-11-28")) {
      no2[hours %in% make_hours(paste0(day, " 00"), paste0(day, " 23"))] <- 40
    }
    CAAQS_no2(data.frame(date = hours, no2 = no2), CAAQS_thresholds())
  }),

  # A fully shuffled input must produce the identical result to the sorted
  # equivalent: the pipeline re-derives calendar structure from the date
  # column, not from row order. With the dense 40 ppb background all three
  # years pass the gates; the annual mean of 40 rounds to one decimal and
  # the 98th percentile of the ordered daily maxima is 40, so the 3-year
  # metric is 40 and the annual level (40 > 7.1) is Red in every window.
  noncontiguous_row_order = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    set.seed(1) # deterministic shuffle
    shuffled <- CAAQS_no2(
      data.frame(date = hours[sample(seq_along(hours))], no2 = 40),
      CAAQS_thresholds()
    )
    sorted <- CAAQS_no2(data.frame(date = hours, no2 = 40), CAAQS_thresholds())
    identical(as.data.frame(shuffled), as.data.frame(sorted)) &&
      identical(sorted$management_level_annual, c("Red", "Red", "Red"))
  }),

  # Pinned contract (no package-wide validation policy yet, see NEWS):
  # every second hour is accepted as input. The GDAD gates then do their
  # work on the filled frame: each calendar day holds only 12 supplied
  # hours (< 18-of-24), so every day is deficient, no daily maximum is
  # valid, and the result is an empty frame -- tolerated input, correctly
  # empty output, no error.
  non_hourly_spacing = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    h2 <- hours[seq(1, length(hours), by = 2)]
    CAAQS_no2(
      data.frame(date = h2, no2 = rep(10, length(h2))), CAAQS_thresholds()
    )
  }),

  # 2022 carries NO o3 values at all (every hour NA). The wrapper warns
  # that 2022 is insufficient and reports only 2021/2023 in the o3 frame:
  # the empty-year contract established when the NA-propagation defect was
  # fixed (no NA rows emitted, 2021/2023 metrics unaffected). The warning
  # itself ("Insufficient data collected for pol: o3 for year(s): 2022
  # ...") is asserted in test-CAAQS-inputs.R; consumers select $o3.
  all_na_year = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    o3[hours >= lubridate::ymd_h("2022-01-01 00") &
      hours < lubridate::ymd_h("2023-01-01 00")] <- NA_real_
    CAAQS(
      dates = hours, o3_1hr_ppb = o3, pm25_1hr_ugm3 = rep(1, length(hours))
    )
  }),

  # With the only supplied pollutant entirely NA, no pollutant has three
  # consecutive complete years, so the wrapper stops with its documented
  # message. Pinned as the contract for a fully-missing pollutant feed.
  all_na_column = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    CAAQS(dates = hours, pm25_1hr_ugm3 = rep(NA_real_, length(hours)))
  })
)
```

Coverage caveats: every completeness and exceptions criterion of the
four guidance documents has at least one fixture except three SO2 cases
(daily-row exceedance retention, the annual-row percentile exception,
and the annual-metric-value relaxed path), omitted as NO2 twins - they
exercise the identical shared code paths and differ only in the
pollutant name and percentile; see fixtures no2_daily_exceedance_retention,
no2_annual_row_exception and no2_50pct_quarter_exception.

Input handling (issue #4): the GDADs legislate no input validation, so
non-contiguous dates, sub-hourly spacing, and NA inputs are pinned as
behaviour contracts (see fixtures noncontiguous_*, non_hourly_spacing,
all_na_*).

Provenance legend: **GDAD** = the expectation follows from quoted
guidance-document wording (cited per fixture); **PIN** = the expectation
pins current behaviour that the guidance does not uniquely determine.
Guidance texts: Ozone GDAD (2021), NO2 GDAD (2020), SO2 GDAD (2020),
PM2.5 GDAD (2012, PN 1483) - CCME Guidance Documents on Achievement
Determination - and the CCME Guidance Document on Air Zone Management
(2019).

Fixture index:

1. `two_of_three_metric_rule` - 2-of-3 metric rule and incomplete-year invalidation
2. `non_consecutive_years` - Non-consecutive years do not break the 3-consecutive-years check
3. `o3_daily_18of24_gate` - O3 daily gate: 18 of 24 valid rolling windows
4. `o3_season_restriction` - O3 ranking restricted to the April 1 - September 30 season
5. `o3_daily_exceedance_retention` - O3 daily-row exception: deficient day retained only on exceedance
6. `o3_annual_row_exception` - O3 annual-row exception: gated-out year retained on exceedance
7. `o3_annual_exception_no_exceedance` - O3 annual-row exception does not retain below-standard years
8. `o3_rounding_cascade_textbox2` - O3 rounding cascade reproduces the GDAD worked example
9. `o3_attribution_cross_midnight` - O3 8-hour windows attributed to their ending hour
10. `no2_98p_ranking` - NO2 98th percentile via the GDAD ranking approach
11. `no2_daily_exceedance_retention` - NO2 daily-row exception: deficient day retained only on exceedance
12. `no2_annual_row_exception` - NO2 annual-row exception: gated-out year's 98th percentile retained on exceedance
13. `no2_50pct_quarter_exception` - NO2 annual metric value exception: relaxed 50%-per-quarter path fires only on exceedance
14. `no2_quarter_days_gate` - NO2 annual gates: 75% of days in the year and 60% of days per quarter
15. `so2_99p_ranking` - SO2 99th percentile via the GDAD ranking approach
16. `pm25_daily_and_annual_gates` - PM2.5 daily 18-of-24 hours and annual 75%/60% gates (no exceptions)
17. `pm25_rounding_cascade` - PM2.5 one-step rounding cascade (Appendix D)
18. `band_edge_classification` - Management-level band edges (Air Zone Management GDAD Appendix 2)
19. `noncontiguous_sparse_days` - Non-contiguous input dates: absent rows are filled, then gated
20. `noncontiguous_row_order` - Non-contiguous input dates: row order does not matter
21. `non_hourly_spacing` - Sub-hourly sampling density is tolerated, not an error
22. `all_na_year` - An all-NA year drops out gracefully with a warning
23. `all_na_column` - An all-NA pollutant column is a clean stop, not a crash

## two_of_three_metric_rule

**2-of-3 metric rule and incomplete-year invalidation** (GDAD)

Rule: GDAD Table 5-3 metric-value rows (all pollutants): a metric value is valid when its annual values "are available for at least two of the possible three" years; a year failing the annual completeness criteria contributes no annual value.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(1, length(hours))
    no2[hours >= lubridate::ymd_h("2024-10-01 00")] <- NA
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
```

Expected: A warning naming 2024 ("Insufficient data collected for pol: no2 ...") is part of the expected behaviour. 2024 has no row; 2023's 3-year window averages 2022 and 2023 only (2 of 3), giving 1; 2021-2022 windows are incomplete and report NA.

Exact expected output (computed):

```r
# result
# A tibble: 3 × 4
   year perc_98_of_daily_maxima `3yr_mean_of_perc_98` management_level_hourly
  <dbl>                   <dbl>                 <dbl> <chr>                  
1  2021                       1                    NA <NA>                   
2  2022                       1                    NA <NA>                   
3  2023                       1                     1 Green                  
```

## non_consecutive_years

**Non-consecutive years do not break the 3-consecutive-years check** (PIN)

Rule: GDAD Table 5-3 metric-value rows: three consecutive years are the averaging window; interior absent years must not poison the check. This fixture pins the bug fixed after the original audit (NA propagation through tidyr::complete()'s implicit years).

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    o3 <- rep(15, length(hours))
    gap <- hours >= lubridate::ymd_h("2022-01-01 00") & hours < 
        lubridate::ymd_h("2023-01-01 00")
    hours <- hours[!gap]
    o3 <- o3[!gap]
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
```

Expected: No error (before the fix: "Cannot calculate CAAQS without at least one pollutant with at least 3 years of complete data"). 2022 has no row; 2024's 3-year window averages 2023 and 2024 (2 of 3).

Exact expected output (computed):

```r
# result
# A tibble: 3 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                   15         NA
2  2023                                   15         NA
3  2024                                   15         15
```

## o3_daily_18of24_gate

**O3 daily gate: 18 of 24 valid rolling windows** (GDAD)

Rule: Ozone GDAD (2021) Table 5-3 (Ozone Dmax 8-hour): "At least 18 (75%) of the 24 O3-8-hr are available in the day"; windows need 6 of 8 hours (section 5.1) and are attributed to their ending hour (eq. 5.1).

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (day in c("04-05", "05-10", "06-15")) {
        o3 <- plateau(hours, o3, paste0("2021-", day), 62)
        o3 <- plateau(hours, o3, paste0("2022-", day), 62)
    }
    o3[hours %in% make_hours("2021-04-20 00", "2021-04-20 11")] <- 55
    o3[hours %in% make_hours("2021-04-20 12", "2021-04-20 23")] <- NA
    o3[hours %in% make_hours("2022-04-20 00", "2022-04-20 23")] <- 55
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
```

Expected: Availability alone flips the 2022 annual fourth highest from the background 10 (deficient day excluded) to 55 (identical day ranked).

Exact expected output (computed):

```r
# result
# A tibble: 3 × 2
   year fourth_highest_daily_max_8hr_mean_o3
  <dbl>                                <dbl>
1  2021                                   10
2  2022                                   55
3  2023                                   10
```

## o3_season_restriction

**O3 ranking restricted to the April 1 - September 30 season** (GDAD)

Rule: Ozone GDAD (2021) Table 5-3 (Annual fourth highest O3 Dmax 8-hour): "O3 Dmax 8-hour are available for at least 75% of the days in the period April 1 to September 30"; section 5.3 restricts ranking to that period.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2023) {
        for (day in c("06-10", "07-15", "08-20", "09-25")) {
            o3 <- plateau(hours, o3, paste0(year, "-", day), 
                50)
        }
        o3 <- plateau(hours, o3, paste0(year, "-03-15"), 58)
        o3 <- plateau(hours, o3, paste0(year, "-10-15"), 58)
    }
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
```

Expected: The fourth highest is 50 (the four in-season elevated days), not 58: off-season daily maxima never rank.

Exact expected output (computed):

```r
# result
# A tibble: 3 × 2
   year fourth_highest_daily_max_8hr_mean_o3
  <dbl>                                <dbl>
1  2021                                   50
2  2022                                   50
3  2023                                   50
```

## o3_daily_exceedance_retention

**O3 daily-row exception: deficient day retained only on exceedance** (GDAD)

Rule: Ozone GDAD (2021) Table 5-3 column 3 (Ozone Dmax 8-hour row): "The O3 Dmax 8-hour exceeds the standard"; section 5.3 worked example: "there are less than eighteen O3-8-hr in a given day and the O3 Dmax 8-hour based on the available data is 70 ppb. Since this O3 Dmax 8-hour exceeds the standard, it will be retained for the selection of the annual fourth highest even though the completeness criterion was not satisfied."

Input:

```r
  hours <- make_hours("2021-01-01 00", "2021-12-31 23")
    o3 <- rep(10, length(hours))
    o3 <- plateau(hours, o3, "2021-05-10", 50)
    o3 <- plateau(hours, o3, "2021-06-10", 51)
    o3 <- plateau(hours, o3, "2021-08-10", 52)
    for (day in c("2021-05-20", "2021-06-20")) {
        o3[hours %in% make_hours(paste0(day, " 00"), paste0(day, 
            " 08"))] <- NA
        o3[hours %in% make_hours(paste0(day, " 17"), paste0(day, 
            " 23"))] <- NA
    }
    o3 <- plateau(hours, o3, "2021-05-20", 70)
    o3 <- plateau(hours, o3, "2021-06-20", 55)
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
```

Expected: Ordered daily maxima: 70 (retained), 52, 51, 50 -> fourth highest 50. Without the exception the retained day would leave only three rankable values (NA fourth highest); had the 55 ppb day also been retained the fourth highest would be 51.

Exact expected output (computed):

```r
# result
# A tibble: 1 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                   50         NA
```

## o3_annual_row_exception

**O3 annual-row exception: gated-out year retained on exceedance** (GDAD)

Rule: Ozone GDAD (2021) Table 5-3 column 3 (Annual fourth highest row): "The annual fourth highest exceeds the standard" - a year failing the season days criterion still contributes its fourth highest when that value, computed on all available data of the year, exceeds the CAAQS in force.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2022) {
        for (day in c("06-10", "07-10")) {
            o3 <- plateau(hours, o3, paste0(year, "-", day), 
                70)
        }
        for (day in c("06-20", "07-20")) {
            o3 <- plateau(hours, o3, paste0(year, "-", day), 
                70)
        }
    }
    o3[hours >= lubridate::ymd_h("2021-08-01 00") & hours < lubridate::ymd_h("2021-10-01 00")] <- NA
    o3[hours >= lubridate::ymd_h("2022-04-01 00") & hours < lubridate::ymd_h("2022-05-16 00")] <- NA
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
```

Expected: 2021 is gated out (66.7% of the season's days) but its fourth highest (70 > the 62 ppb CAAQS in force) is retained; 2022 is gated in (91.8% of the season's days). 2023's 3-year window averages 70, 70, 10 -> 50. A warning naming 2021 ("Insufficient data ...") accompanies the CAAQS() call, not this pipeline call; the below-standard control is fixture o3_annual_exception_no_exceedance.

Exact expected output (computed):

```r
# result
# A tibble: 3 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                   70         NA
2  2022                                   70         NA
3  2023                                   10         50
```

## o3_annual_exception_no_exceedance

**O3 annual-row exception does not retain below-standard years** (GDAD)

Rule: Ozone GDAD (2021) Table 5-3 column 3 (Annual fourth highest row): the exception's condition is the exceedance itself; below-standard values of gated-out years are not retained.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2022) {
        for (day in c("06-10", "06-20", "07-10", "07-20")) {
            o3 <- plateau(hours, o3, paste0(year, "-", day), 
                50)
        }
    }
    o3[hours >= lubridate::ymd_h("2021-08-01 00") & hours < lubridate::ymd_h("2021-10-01 00")] <- NA
    o3[hours >= lubridate::ymd_h("2022-04-01 00") & hours < lubridate::ymd_h("2022-05-16 00")] <- NA
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
```

Expected: Identical gaps and day counts as o3_annual_row_exception but plateaus at 50 (< 62): 2021 drops out entirely (below-standard gated-out years are not retained), 2022 is gated in at 50, and 2023's 3-year window has only one annual value, so it reports NA.

Exact expected output (computed):

```r
# result
# A tibble: 2 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2022                                   50         NA
2  2023                                   10         NA
```

## o3_rounding_cascade_textbox2

**O3 rounding cascade reproduces the GDAD worked example** (GDAD)

Rule: Ozone GDAD (2021) Table 5-4: O3 Dmax 8-hour reported to one decimal place; metric value (3-year average) reported as a whole number via the two-step procedure. Text Box 2: annual fourth highest of 72.5, 60.5 and 55.9 ppb -> 3-year average 62.966... -> "62.9 ppb is rounded upward 63 ppb, which is the calculated ozone CAAQS metric value."

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2023) {
        target <- c(`2021` = 72.5, `2022` = 60.5, `2023` = 55.9)[[as.character(year)]]
        for (day in c("06-10", "06-20", "07-10", "07-20")) {
            o3 <- plateau(hours, o3, paste0(year, "-", day), 
                target)
        }
    }
    CAAQS_o3(data.frame(date = hours, o3 = o3), CAAQS_thresholds())
```

Expected: Four plateau days per year put each target at the annual fourth highest (the background 10 ranks fifth). The 3-year average of 72.5, 60.5, 55.9 is 62.966... -> whole number 63.

Exact expected output (computed):

```r
# result
# A tibble: 3 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                 72.5         NA
2  2022                                 60.5         NA
3  2023                                 55.9         63
```

## o3_attribution_cross_midnight

**O3 8-hour windows attributed to their ending hour** (GDAD)

Rule: Ozone GDAD (2021) eq. 5.1: the O3-8-hr for hour J is the mean of the 8-hour period ENDING at J and "is assigned to that ending hour". Daily maxima are taken over the 24 windows ending in each day.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    o3 <- rep(10, length(hours))
    for (year in 2021:2023) {
        for (day in c("05-10", "06-10", "07-10")) {
            o3[hours %in% make_hours(paste0(year, "-", day, " 16"), 
                paste0(year, "-", day, " 23"))] <- 100
        }
    }
    CAAQS(dates = hours, o3_1hr_ppb = o3)$o3
```

Expected: 2021-2023 daily maxima include three 100-ppb days (the plateau window ending 23:00) and three 88.8 cross-midnight days (the window ending 00:00 next day: (7 x 100 + 10)/8 = 88.75 -> 88.8 per Table 5-4), so the annual fourth highest is 88.8; attributing windows to their START hour instead would fold the spillover window into the plateau day and leave a fourth highest of 10. 2024 is background (fourth highest 10). 3-year metric values: 2023's window (88.8 + 88.8 + 88.8)/3 = 88.8 -> 89 and 2024's (88.8 + 88.8 + 10)/3 = 62.533... -> 63 (whole number, Table 5-4).

Exact expected output (computed):

```r
# result
# A tibble: 4 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                 88.8         NA
2  2022                                 88.8         NA
3  2023                                 88.8         89
4  2024                                 10           63
```

## no2_98p_ranking

**NO2 98th percentile via the GDAD ranking approach** (GDAD)

Rule: NO2 GDAD (2020) Appendix B: the 98th percentile is the Kth highest daily maximum with K = NDM - Trunc(NDM x 0.98) (NDM = 365 -> 8th highest), ties repeated in rank order; footnote forbids interpolation (type 7).

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(1, length(hours))
    for (year in 2021:2023) {
        for (d in 1:7) {
            no2[hours %in% make_hours(paste0(year, "-06-0", d, 
                " 14"), paste0(year, "-06-0", d, " 14"))] <- 100
        }
    }
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    for (year in 2021:2023) {
        no2[hours %in% make_hours(paste0(year, "-07-02 08"), 
            paste0(year, "-07-02 08"))] <- 100
    }
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    list(seven_spikes = out, eight_spikes = out2)
```

Expected: seven_spikes: perc_98 = 1 (the 8th highest of 365 daily maxima, ties counted in rank order). eight_spikes: perc_98 = 100 (the eighth spike day becomes the 8th highest).

Exact expected output (computed):

```r
# seven_spikes
# A tibble: 3 × 4
   year perc_98_of_daily_maxima `3yr_mean_of_perc_98` management_level_hourly
  <dbl>                   <dbl>                 <dbl> <chr>                  
1  2021                       1                    NA <NA>                   
2  2022                       1                    NA <NA>                   
3  2023                       1                     1 Green                  
# eight_spikes
# A tibble: 3 × 4
   year perc_98_of_daily_maxima `3yr_mean_of_perc_98` management_level_hourly
  <dbl>                   <dbl>                 <dbl> <chr>                  
1  2021                     100                    NA <NA>                   
2  2022                     100                    NA <NA>                   
3  2023                     100                   100 Red                    
```

## no2_daily_exceedance_retention

**NO2 daily-row exception: deficient day retained only on exceedance** (GDAD)

Rule: NO2 GDAD (2020) Table 5-3 column 3 (NO2 Dmax 1-hour row): "The NO2 Dmax 1-hour exceeds the standard" (daily row: "At least 18 of the 24 (75%) NO2 1-hour are available in the day").

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(1, length(hours))
    for (year in 2021:2023) {
        for (d in 1:7) {
            no2[hours %in% make_hours(paste0(year, "-06-0", d, 
                " 14"), paste0(year, "-06-0", d, " 14"))] <- 100
        }
    }
    no2[hours %in% make_hours("2021-07-01 00", "2021-07-01 23")] <- NA
    no2[hours %in% make_hours("2021-07-01 08", "2021-07-01 08")] <- 500
    no2[hours %in% make_hours("2022-07-01 00", "2022-07-01 23")] <- NA
    no2[hours %in% make_hours("2022-07-01 08", "2022-07-01 08")] <- 50
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
```

Expected: perc_98 = (100, 1, 1): the exceeding deficient day is retained, the below-standard one is not. (Retaining 2022's day would give 50.)

Exact expected output (computed):

```r
# result
# A tibble: 3 × 3
   year perc_98_of_daily_maxima management_level_hourly
  <dbl>                   <dbl> <chr>                  
1  2021                     100 <NA>                   
2  2022                       1 <NA>                   
3  2023                       1 Orange                 
```

## no2_annual_row_exception

**NO2 annual-row exception: gated-out year's 98th percentile retained on exceedance** (GDAD)

Rule: NO2 GDAD (2020) Table 5-3 column 3 (Annual 98th percentile row): "The 98th percentile based on the available NO2 Dmax 1-hour exceeds the standard"; the annual metric value row's exception ("1. at least 50% of the NO2 1-hour are available in each calendar quarter; and 2. the annual average exceeds the standard") does NOT fire here (Q4 holds no hours), so 2024's annual mean is NA even though its percentile is retained.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(5, length(hours))
    for (year in 2021:2024) {
        for (d in 1:8) {
            no2[hours %in% make_hours(paste0(year, "-06-0", d, 
                " 14"), paste0(year, "-06-0", d, " 14"))] <- 100
        }
    }
    no2[hours >= lubridate::ymd_h("2024-09-15 00") & hours < 
        lubridate::ymd_h("2025-01-01 00")] <- NA
    CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
```

Expected: 2024 is gated out (a warning names 2024) but retained under the percentile-row exception: perc_98 = 100 > the 60 ppb CAAQS in force. annual_mean = NA for 2024 (Q4 holds no hours). The 2023 and 2024 3-year percentile averages are both (100 + 100 + 100)/3 = 100.

Exact expected output (computed):

```r
# result
# A tibble: 4 × 5
   year perc_98_of_daily_maxima annual_mean_of_hourly `3yr_mean_of_perc_98` management_level_hourly
  <dbl>                   <dbl>                 <dbl>                 <dbl> <chr>                  
1  2021                     100                   5.1                    NA <NA>                   
2  2022                     100                   5.1                    NA <NA>                   
3  2023                     100                   5.1                   100 Red                    
4  2024                     100                  NA                     100 Red                    
```

## no2_50pct_quarter_exception

**NO2 annual metric value exception: relaxed 50%-per-quarter path fires only on exceedance** (GDAD)

Rule: NO2 GDAD (2020) Table 5-3 column 3 (Annual metric value row): "1. at least 50% of the NO2 1-hour are available in each calendar quarter; and 2. the annual average exceeds the standard" - the relaxed criterion replaces the 75%/60% annual gates when the annual average exceeds the CAAQS.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(20, length(hours))
    no2[hours >= lubridate::ymd_h("2024-10-01 00") & hours <= 
        lubridate::ymd_h("2024-11-15 23")] <- NA
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    no2b <- rep(5, length(hours))
    no2b[hours >= lubridate::ymd_h("2024-10-01 00") & hours <= 
        lubridate::ymd_h("2024-11-15 23")] <- NA
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2b)$no2
    list(exceeds = out, below = out2)
```

Expected: exceeds: 2024 retained (annual_mean = 20 > 17); its 98th percentile of 20 does NOT exceed the 60 ppb hourly CAAQS, so perc_98 = NA for 2024. below: 2024 has no row (years 2021-2023 only).

Exact expected output (computed):

```r
# exceeds
# A tibble: 4 × 5
   year perc_98_of_daily_maxima annual_mean_of_hourly management_level_hourly management_level_ann…¹
  <dbl>                   <dbl>                 <dbl> <chr>                   <chr>                 
1  2021                      20                    20 <NA>                    Red                   
2  2022                      20                    20 <NA>                    Red                   
3  2023                      20                    20 Green                   Red                   
4  2024                      NA                    20 Green                   Red                   
# ℹ abbreviated name: ¹​management_level_annual
# below
# A tibble: 3 × 5
   year perc_98_of_daily_maxima annual_mean_of_hourly management_level_hourly management_level_ann…¹
  <dbl>                   <dbl>                 <dbl> <chr>                   <chr>                 
1  2021                       5                     5 <NA>                    Yellow                
2  2022                       5                     5 <NA>                    Yellow                
3  2023                       5                     5 Green                   Yellow                
# ℹ abbreviated name: ¹​management_level_annual
```

## no2_quarter_days_gate

**NO2 annual gates: 75% of days in the year and 60% of days per quarter** (GDAD)

Rule: NO2 GDAD (2020) Table 5-3 (Annual 98th percentile row): the daily maxima "are available for at least: 1. 75% of the days in a year; and 2. 60% of the days in each calendar quarter".

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    no2 <- rep(1, length(hours))
    no2[hours >= lubridate::ymd_h("2024-10-01 00") & hours < 
        lubridate::ymd_h("2024-11-10 00")] <- NA
    out <- CAAQS(dates = hours, no2_1hr_ppb = no2)$no2
    no2b <- rep(1, length(hours))
    no2b[hours >= lubridate::ymd_h("2024-10-01 00") & hours < 
        lubridate::ymd_h("2024-10-31 00")] <- NA
    out2 <- CAAQS(dates = hours, no2_1hr_ppb = no2b)$no2
    list(fails = out, passes = out2)
```

Expected: fails: 2024 has no row (a warning names 2024); 2023's 3-year window covers 2021-2023, all complete, and reports 1. passes: all four years present.

Exact expected output (computed):

```r
# fails
# A tibble: 3 × 3
   year perc_98_of_daily_maxima `3yr_mean_of_perc_98`
  <dbl>                   <dbl>                 <dbl>
1  2021                       1                    NA
2  2022                       1                    NA
3  2023                       1                     1
# passes
# A tibble: 4 × 3
   year perc_98_of_daily_maxima `3yr_mean_of_perc_98`
  <dbl>                   <dbl>                 <dbl>
1  2021                       1                    NA
2  2022                       1                    NA
3  2023                       1                     1
4  2024                       1                     1
```

## so2_99p_ranking

**SO2 99th percentile via the GDAD ranking approach** (GDAD)

Rule: SO2 GDAD (2020) Appendix B: the 99th percentile is the Kth highest daily maximum with K = NDM - Trunc(NDM x 0.99); with NDM = 365 the floating-point product 365 x 0.99 = 361.3499... truncates cleanly, and for NDM = 100 the product 100 x 0.99 = 98.999... (a floating-point near-miss of 99) must truncate to 99, so K = 1: the CAAQS_rank_percentile tolerance guards exactly this.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    so2 <- rep(1, length(hours))
    for (year in 2021:2023) {
        for (d in 1:4) {
            so2[hours %in% make_hours(paste0(year, "-06-0", d, 
                " 14"), paste0(year, "-06-0", d, " 14"))] <- 100
        }
    }
    out <- CAAQS(dates = hours, so2_1hr_ppb = so2)$so2
    so2b <- rep(1, length(hours))
    for (year in 2021:2023) {
        for (d in 1:3) {
            so2b[hours %in% make_hours(paste0(year, "-06-0", 
                d, " 14"), paste0(year, "-06-0", d, " 14"))] <- 100
        }
    }
    out2 <- CAAQS(dates = hours, so2_1hr_ppb = so2b)$so2
    list(four_spikes = out, three_spikes = out2)
```

Expected: four_spikes: perc_99 = 100 (the 4th highest of 365 daily maxima). three_spikes: perc_99 = 1 (the 4th highest is background). stats::quantile() (type 7) would return 1 for four_spikes, missing all four exceedance days.

Exact expected output (computed):

```r
# four_spikes
# A tibble: 3 × 4
   year perc_99_of_daily_maxima `3yr_mean_of_perc_99` management_level_annual
  <dbl>                   <dbl>                 <dbl> <chr>                  
1  2021                     100                    NA Green                  
2  2022                     100                    NA Green                  
3  2023                     100                   100 Green                  
# three_spikes
# A tibble: 3 × 4
   year perc_99_of_daily_maxima `3yr_mean_of_perc_99` management_level_annual
  <dbl>                   <dbl>                 <dbl> <chr>                  
1  2021                       1                    NA Green                  
2  2022                       1                    NA Green                  
3  2023                       1                     1 Green                  
```

## pm25_daily_and_annual_gates

**PM2.5 daily 18-of-24 hours and annual 75%/60% gates (no exceptions)** (GDAD)

Rule: PM2.5 GDAD (2012, PN 1483) section 4.1.4: a daily 24hr-PM2.5 is valid when "at least 75% (18 hours) of the 1-hour concentrations are available on the given day"; annual 98P and annual average require "at least 75% valid daily-24hr-PM2.5 in the year" and "at least 60% ... in each calendar quarter" (sections 4.1.4/4.2.4). The 2012 document has no exceptions column: a gated-out year contributes nothing even when its values exceed the standard.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2024-12-31 23")
    pm25 <- rep(5, length(hours))
    for (d in 1:7) {
        pm25[hours %in% make_hours(paste0("2021-06-0", d, " 00"), 
            paste0("2021-06-0", d, " 23"))] <- 26
    }
    pm25[hours %in% make_hours("2021-06-08 00", "2021-06-08 23")] <- 26
    pm25[hours %in% make_hours("2021-06-08 09", "2021-06-08 15")] <- NA
    pm25[hours >= lubridate::ymd_h("2024-07-01 00")] <- NA
    out <- CAAQS(dates = hours, pm25_1hr_ugm3 = pm25)$pm25
    pm25c <- pm25
    pm25c[hours %in% make_hours("2021-06-08 09", "2021-06-08 15")] <- 26
    out2 <- CAAQS(dates = hours, pm25_1hr_ugm3 = pm25c)$pm25
    list(deficient_day = out, complete_day = out2)
```

Expected: deficient_day: 2021's perc_98 = 5 (7 valid spike days rank 1-7; the 17-hour day is invalid) and 2024 has no row (a warning names 2024). complete_day: the deficient day is valid too, so 2021's ranking-approach 98P of 365 daily means (365 - Trunc(0.98 x 365) = 8th highest) is 26.

Exact expected output (computed):

```r
# deficient_day
# A tibble: 3 × 5
   year perc_98_of_daily_means mean_of_daily_means `3yr_mean_of_perc_98` management_level_daily
  <dbl>                  <dbl>               <dbl>                 <dbl> <chr>                 
1  2021                      5                 5.4                    NA <NA>                  
2  2022                      5                 5                      NA <NA>                  
3  2023                      5                 5                       5 Green                 
# complete_day
# A tibble: 3 × 5
   year perc_98_of_daily_means mean_of_daily_means `3yr_mean_of_perc_98` management_level_daily
  <dbl>                  <dbl>               <dbl>                 <dbl> <chr>                 
1  2021                     26                 5.5                    NA <NA>                  
2  2022                      5                 5                      NA <NA>                  
3  2023                      5                 5                      12 Yellow                
```

## pm25_rounding_cascade

**PM2.5 one-step rounding cascade (Appendix D)** (GDAD)

Rule: PM2.5 GDAD (2012, PN 1483) Appendix D: "numbers with second decimal .05 will be rounded upward" (one-step half-up); daily means, annual averages and metric values reported to one decimal place (sections 4.1.1, 4.2.2, 4.2.3). The daily means round BEFORE the annual average.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    pm25 <- rep(20.1, length(hours))
    pm25[hours >= lubridate::ymd_h("2023-01-01 00")] <- 20.2
    CAAQS_pm25(data.frame(date = hours, pm25 = pm25), CAAQS_thresholds())
```

Expected: Annual means (20.1, 20.1, 20.2); 3-year average (20.1 + 20.1 + 20.2)/3 = 20.133... -> second decimal 3 (< .05) -> 20.1.

Exact expected output (computed):

```r
# result
# A tibble: 3 × 3
   year mean_of_daily_means `3yr_mean_of_means`
  <dbl>               <dbl>               <dbl>
1  2021                20.1                NA  
2  2022                20.1                NA  
3  2023                20.2                20.1
```

## band_edge_classification

**Management-level band edges (Air Zone Management GDAD Appendix 2)** (GDAD)

Rule: Guidance Document on Air Zone Management (2019) Appendix 2, Table A2-1: O3 2020 levels "Red > 62 ppb; Orange 57 to 62 ppb; Yellow 51 to 56 ppb; Green < 50 ppb". The achievement GDADs state the metric "is achieved if ... less than or equal to the standard", so Red is strict > and the Orange/Yellow lower edges are inclusive.

Input:

```r
  th <- CAAQS_thresholds()$o3$`8hr`
    values <- c(62, 62.00001, 57, 56.9, 51, 50.9)
    out <- vapply(values, function(v) CAAQS_meets_standard(2022, 
        v, th), character(1))
    setNames(unname(out), values)
```

Expected: 62 -> Orange; 62.00001 -> Red; 57 -> Orange; 56.9 -> Yellow; 51 -> Yellow; 50.9 -> Green. (2022 thresholds: Red 62, Orange 57, Yellow 51.)

Exact expected output (computed):

```r
# result
      62 62.00001       57     56.9       51     50.9 
"Orange"    "Red" "Orange" "Yellow" "Yellow"  "Green" 
```

## noncontiguous_sparse_days

**Non-contiguous input dates: absent rows are filled, then gated** (GDAD)

Rule: No GDAD rule forbids sparse input: absent dates are treated as missing hours. The NO2 GDAD (2020) Table 5-3 days criteria then gate the result: 2022 supplies only four isolated days (2022-01-15, 2022-03-03, 2022-07-09, 2022-11-28) at 40 ppb, so 4/365 days (1.1%) < 75% of days and each quarter holds 1 day (~1%) < 60%: 2022 contributes no metric rows. 2021/2023 are complete at 40 ppb.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    no2 <- rep(40, length(hours))
    no2[hours >= lubridate::ymd_h("2022-01-01 00") & hours < 
        lubridate::ymd_h("2023-01-01 00")] <- NA_real_
    for (day in c("2022-01-15", "2022-03-03", "2022-07-09", "2022-11-28")) {
        no2[hours %in% make_hours(paste0(day, " 00"), paste0(day, 
            " 23"))] <- 40
    }
    CAAQS_no2(data.frame(date = hours, no2 = no2), CAAQS_thresholds())
```

Expected: Rows only for 2021 and 2023, each perc_98 = 40 (the ordered daily maxima are all 40). The all-NA 2022 hours are filled rows, not input errors.

Exact expected output (computed):

```r
# result
# A tibble: 2 × 2
   year perc_98_of_daily_maxima
  <dbl>                   <dbl>
1  2021                      40
2  2023                      40
```

## noncontiguous_row_order

**Non-contiguous input dates: row order does not matter** (PIN)

Rule: The pipeline re-derives calendar structure from the date column, not from row order: a fully shuffled input produces the identical result to the sorted equivalent. Pinned contract (no package-wide validation policy yet); with the dense 40 ppb background all three years pass the gates, and the annual level (40 > 7.1 ppb) is Red in every window.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    set.seed(1)
    shuffled <- CAAQS_no2(data.frame(date = hours[sample(seq_along(hours))], 
        no2 = 40), CAAQS_thresholds())
    sorted <- CAAQS_no2(data.frame(date = hours, no2 = 40), CAAQS_thresholds())
    identical(as.data.frame(shuffled), as.data.frame(sorted)) && 
        identical(sorted$management_level_annual, c("Red", "Red", 
            "Red"))
```

Expected: TRUE: shuffled and sorted inputs agree row-for-row and level-for-level.

Exact expected output (computed):

```r
# result
[1] TRUE
```

## non_hourly_spacing

**Sub-hourly sampling density is tolerated, not an error** (GDAD)

Rule: Pinned contract (no package-wide validation policy yet): every second hour is accepted as input. The NO2 GDAD Table 5-3 daily criterion then does the work: each calendar day holds only 12 supplied hours (< 18-of-24), so every day is deficient and the result is an empty frame - tolerated input, correctly empty output.

Input:

```r
  hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    h2 <- hours[seq(1, length(hours), by = 2)]
    CAAQS_no2(data.frame(date = h2, no2 = rep(10, length(h2))), 
        CAAQS_thresholds())
```

Expected: A zero-row frame: no day reaches the 18-of-24 valid-hours criterion.

Exact expected output (computed):

```r
# result
# A tibble: 0 × 2
# ℹ 2 variables: year <dbl>, perc_98_of_daily_maxima <dbl>
```

## all_na_year

**An all-NA year drops out gracefully with a warning** (PIN)

Rule: 2022 carries no o3 values at all. The wrapper warns that 2022 is insufficient and reports only 2021/2023 in the o3 frame: the empty-year contract established with the NA-propagation fix (no NA rows emitted, neighbouring years unaffected).

Input:

```r
suppressWarnings({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    o3 <- rep(10, length(hours))
    o3[hours >= lubridate::ymd_h("2022-01-01 00") & hours < lubridate::ymd_h("2023-01-01 00")] <- NA_real_
    CAAQS(dates = hours, o3_1hr_ppb = o3, pm25_1hr_ugm3 = rep(1, 
        length(hours)))
})$o3
```

Expected: Rows only for 2021/2023, each fourth-highest 10 and 3-year mean NA (two years in the 2021-2023 window).

Exact expected output (computed):

```r
# result
# A tibble: 2 × 3
   year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean`
  <dbl>                                <dbl>      <dbl>
1  2021                                   10         NA
2  2023                                   10         NA
```

## all_na_column

**An all-NA pollutant column is a clean stop, not a crash** (PIN)

Rule: With the only supplied pollutant entirely NA, no pollutant has three consecutive complete years, so the wrapper stops with its documented message. Pinned as the contract for a fully-missing pollutant feed.

Input:

```r
tryCatch({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    CAAQS(dates = hours, pm25_1hr_ugm3 = rep(NA_real_, length(hours)))
}, error = function(e) conditionMessage(e))
```

Expected: The message string: Cannot calculate CAAQS without at least one pollutant with at least 3 years of complete data.

Exact expected output (computed):

```r
# result
[1] "Cannot calculate CAAQS without at least one pollutant with at least 3 years of complete data."
```

