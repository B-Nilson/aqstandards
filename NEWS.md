# aqstandards 0.1.0

## CAAQS

Audit of `CAAQS()` against current CCME guidance (issue #3). The
standards table (Red thresholds and effective years: O3 63/62/60 ppb,
PM2.5 28/27 ug/m3 daily and 10/8.8 ug/m3 annual, NO2 60/42 ppb hourly
and 17/12 ppb annual, SO2 70/65 ppb hourly and 5/4 ppb annual for
2015/2020/2025 as applicable) is confirmed current as of 2025-08.

- The O3 8-hour metric now uses rolling 8-hour means per the CCME
  Guidance Document on Achievement Determination for Ozone (2021),
  instead of three non-overlapping 8-hour blocks per day. Daily maxima
  no longer miss peaks straddling block boundaries. Verified against
  the guidance text (GDAD eq. 5.1, Table 5-3): each rolling window
  needs at least six of the eight 1-hour values (so series starts use
  6- or 7-hour averages), and windows are assigned to the hour ending
  the averaging period, so each day's maximum is taken over the 24
  windows ending in that day.
- The NO2 and SO2 annual 98th/99th percentiles of daily maxima are now
  computed with the GDAD percentile ranking approach (NO2 GDAD 2020
  Appendix B; SO2 GDAD 2020): the Kth highest value with
  K = NDM - Truncated(NDM x p), where NDM is the number of daily
  maxima available in the year. This replaces stats::quantile()
  (type 7), which interpolates between order statistics and can return
  values that were never measured; the GDAD only permits other methods
  when they always reproduce the ranking approach. A small tolerance
  guards the truncation against floating-point products such as
  100 x 0.99. The rank-percentile and interpolated definitions agree
  for complete years (351-366 daily maxima), but differ for incomplete
  years and mid-window values, so management levels can change where
  data are incomplete.
- Per the GDAD completeness tables (Table 5-3), 3-year metric values
  for O3, NO2 and SO2 are now computed when at least two of the three
  annual values are available, instead of requiring all three. This is
  a user-visible behaviour change: years whose 3-year window contains
  exactly two complete years now report a metric and management level
  instead of NA.
- Fixed swapped metrics in the NO2 and SO2 management levels: the hourly
  management level is now computed from the 3-year average of the annual
  98th (NO2) / 99th (SO2) percentile of daily maximum 1-hour
  concentrations, and the annual management level from the annual mean
  of hourly concentrations, as the CCME standards define. Previously
  each metric was compared against the other averaging period's
  thresholds.
- Years with incomplete 3-year averaging windows no longer report a
  management level for metrics based on 3-year averages: the level is
  NA instead of a classification computed from a partial window (which
  could, for example, classify a site "Green" from a single year of
  data). This is a user-visible behaviour change; use the most recent
  three complete years to reproduce previous partial-window output.
- Documented metric provenance and the local standard time assumption in
  the function help and on the threshold table. Interpretation choice:
  hourly input timestamps are treated as labelling the start of the
  averaging hour (so hour-ending values map directly onto hourly rows).
- Data completeness is now assessed with the pollutant-specific criteria
  of the guidance documents (Table 5-3 of each GDAD) instead of the
  former uniform annual 50% hourly-availability heuristic. Annual metric
  values are reported only for years in which:
  - O3: at least 75% of the days from April 1 to September 30 have a
    valid daily maximum 8-hour rolling average; the annual fourth-highest
    is ranked only over the April 1 - September 30 season (Ozone GDAD
    2021, Table 5-3 and section 5.3); and within each day at least 18 of
    the 24 rolling 8-hour averages were available.
  - NO2 and SO2: the daily maxima fed to the annual 98th (99th)
    percentile come only from days with at least 18 of 24 hours
    available; the year must have valid daily maxima on at least 75% of
    its days and 60% of the days in each calendar quarter (NO2 GDAD 2020
    and SO2 GDAD 2020, Table 5-3); and the annual-mean metric additionally
    requires 75% of hours available in the year and 60% in each quarter.
  - PM2.5: daily 24hr-PM2.5 values are computed only for days with at
    least 75% (18) of the 1-hour concentrations available, and the annual
    98th percentile and annual average are reported only for years with
    valid daily values on at least 75% of the year's days and 60% of the
    days in each calendar quarter (PM2.5 GDAD 2012, PN 1483, sections
    4.1.4 and 4.2.4). Unlike NO2/SO2 there is no hours-per-year criterion:
    the annual gates are expressed in valid days only. PM2.5 joins the
    same completeness configuration (CAAQS_completeness()) and gate
    logic as the other pollutants.
  The daily-row exceptions of Table 5-3 are now implemented: a day that
  fails its daily criterion is still retained when its value exceeds the
  standard, per the column-3 criteria "The O3 Dmax 8-hour exceeds the
  standard", "The NO2 Dmax 1-hour exceeds the standard" and "The SO2 Dmax
  1-hour exceeds the standard" (with each GDAD's section 5.3 worked
  example; the comparison uses the Red management level, i.e. the CAAQS
  in force in the day's year). This is a user-visible behaviour change:
  deficient days previously dropped are now retained when their value
  exceeds the standard, which can only raise a metric value.
  The annual-row exceptions of Table 5-3 are now implemented as well:
  a year that fails its annual completeness criteria still contributes
  its annual fourth highest (O3: "The annual fourth highest exceeds the
  standard") or its annual 98th/99th percentile (NO2/SO2: "The 98th/99th
  percentile based on the available NO2/SO2 Dmax 1-hour exceeds the
  standard") when that value, computed on all available data of the
  year, exceeds the CAAQS in force in that year; and the NO2/SO2 annual
  average is retained under its annual metric value row's exception when
  at least 50% of the 1-hour values are available in each calendar
  quarter (relaxing the 75%/60% criteria) and the annual average exceeds
  the standard. Years retained by no exception contribute nothing, and a
  year's metric value never depends on the year-level completeness gates
  (the gates and exceptions decide only whether it is reported). This is
  a user-visible behaviour change: gated-out years previously dropped
  silently can now report a metric value and, through the 2-of-3 rule,
  affect 3-year averages. PM2.5's 2012 GDAD predates the Table 5-3
  format and has no data-completeness exceptions column (its
  "exceptional events" procedures are an administrative TF/EE
  designation process, not a data-completeness rule), so none are
  implemented for PM2.5.
- The `min_completeness` argument has been removed from `CAAQS()`: every
  pollutant now follows its guidance document's completeness gates, so
  the last heuristic (a uniform annual availability fraction, previously
  retained for PM2.5) is retired. This is a user-visible behaviour
  change: calls passing `min_completeness` now fail with an "unused
  argument" error, and PM2.5 years are judged by the PM2.5 GDAD criteria
  (75% of days in the year, 60% in each quarter) rather than a
  user-set hourly-availability fraction.
- Hours-per-year requirements are derived from the calendar via
  lubridate's leap-year rule instead of the previously hardcoded
  `year %% 4` check, which mishandled century years such as 2100.
- The PM2.5 annual 98th percentile is computed with the GDAD percentile
  ranking approach (PM2.5 GDAD 2012, PN 1483, section 4.1.2, Steps 1-3):
  the (N - Trunc(N x 0.98))th highest daily 24hr-PM2.5 (worked example
  N = 275 -> 6th highest), replacing stats::quantile() (type 7), which
  footnote 11 of the same document explicitly disallows. The PM2.5
  annual average is the mean of the valid daily-24hr-PM2.5 values
  (section 4.2.2, Equation 3), and a 3-year metric value is valid when
  its annual values are available for at least two of the required
  three years (sections 4.1.4/4.2.4).
- Fixed an error when the data span non-consecutive years: a year absent
  from the data (for example 2022 in a 2021-2024 series) no longer
  propagates NA through the three-consecutive-years completeness check.
- Metric values are now rounded per the GDADs' decimal-place and rounding
  rules before comparison to a standard or management level (Ozone GDAD
  2021 Table 5-4; NO2 and SO2 GDADs 2020 Table 5-4; PM2.5 GDAD 2012, PN
  1483, Appendix D with sections 4.1.1, 4.1.3, 4.2.2 and 4.2.3): one
  decimal place for the daily maxima and annual percentiles of O3, NO2
  and SO2 and for PM2.5's daily means, annual averages and metric
  values; whole numbers for the O3 metric value and the NO2/SO2 1-hour
  metric values. The rounding is half-up (the GDADs' two-step
  "discard-then-round" procedure and PM2.5's Appendix D convention both
  round upward exactly when the first discarded digit is 5 or more; the
  Ozone GDAD's worked example, 3-year average 62.966... ppb reported as
  63 ppb, is reproduced end-to-end). This is a user-visible behaviour
  change: reported metric values now carry their GDAD-specified
  precision, and values previously compared unrounded can classify
  differently at band boundaries.
- Management levels are now assigned with the real band edges of the
  CCME Guidance Document on Air Zone Management (2019, Appendix 2,
  Tables A2-1 to A2-4) and its comparison semantics: Red is strict `>`
  (the achievement determination GDADs' "less than or equal to the
  standard" rule), the Orange and Yellow lower edges are inclusive
  ("32 to 60 ppb", "21 to 31 ppb", ...), and Green is everything below
  the Yellow lower edge. The threshold table no longer stores Yellow
  and Orange values offset by 0.01 to emulate inclusive edges with
  strict `>` comparisons: with GDAD-rounded metrics the two schemes
  agree on every reachable value, and the table now carries the CCME's
  own numbers (for example O3 2020 Orange 57 and Yellow 51 instead of
  56.01 and 50.01). Appendix 2 also mandates the rounding above: "the
  metric values for comparison to the concentrations must be rounded to
  the same number of digits as the shown concentrations".
- Documented the CAAQS methodology in the README (per-pollutant metric
  definitions, completeness criteria, exceptions and sources, replacing
  that section's TODO placeholder) and made the README and `CAAQS()`
  examples deterministic (exact inputs instead of unseeded `sample()`).
  Added `data-raw/CAAQS-regression-fixtures.R`, whose execution writes
  `data-raw/CAAQS-regression-fixtures.md`: a catalogue of 18
  deterministic rule-level fixtures (minimal synthetic inputs with exact
  values and dates, plus expected outputs computed by running the
  package) covering every implemented rule - daily/annual/quarterly
  gates, the O3 season, daily-row and annual-row exceptions, the
  relaxed 50%-per-quarter path, the 2-of-3 metric rule, GDAD percentile
  ranking, rounding cascades, cross-midnight window attribution and
  band-edge classification - each marked as GDAD-derived or
  current-behaviour-pinning, for the planned regression-test issue;
  re-running the generator reproduces the document byte-for-byte. The
  document embeds the scenario helpers it uses (owned by
  `tests/testthat/helper-CAAQS.R`, shared with the tests), records the
  coverage caveats, and a test asserts the committed document matches a
  fresh regeneration.
- Extended the fixture catalogue to 23 fixtures and added
  `tests/testthat/test-CAAQS-inputs.R` for the input-handling behaviour
  the guidance documents do not legislate (the issue #4 matrix gaps):
  non-contiguous input dates are accepted and filled, then gated by the
  GDAD criteria; row order does not matter; sub-hourly sampling density
  is tolerated (and correctly gates to an empty result); an all-NA year
  warns and drops out; an all-NA pollutant column stops with the
  documented message. **Interpretation choice:** no new input-validation
  stops were added and current tolerant behaviour is pinned as the
  contract - the issue defers a package-wide validation policy, which
  remains open for the planned standards-data architecture refactor.
  Mismatched input lengths surface the underlying recycling error
  (pinned, not wrapped).

## AQHI+

First stable release of the `AQHI_plus()` public contract for downstream
package use (e.g. `B-Nilson/lcm-evaluation`). The implementation follows
Yao et al. (2019) and current ECCC guidance.

- Documented scientific contract: input is the 1-hour mean PM2.5 in
  ug/m3; AQHI+ levels are 1-10 in right-closed 10 ug/m3 bins, with "+"
  above 100 ug/m3; health risk boundaries are Low (<= 30 ug/m3 / levels
  1-3), Moderate (>30-60 / levels 4-6), High (>60-100 / levels 7-10),
  and Very High (>100 / "+").
- Explicit, documented invalid-input policy: NA input stays missing;
  NaN, Inf, -Inf and concentrations below `min_allowed_pm25` (negative
  values by default) are treated as missing (NA) rather than emerging
  accidentally from binning.
- The display representation is preserved: 10+ is reported as "+", never
  as an invented numeric level 11.
- Added regression tests for the health-relevant concentration
  boundaries (29.9/30/30.1, 59.9/60/60.1, 99.9/100/100.1), zero and low
  positive values, negative and non-finite inputs, English and French
  risk categories, and both level-only and detailed output paths.
- Behavioural changes to the AQHI+ mapping in future releases require a
  version bump, regression tests, and release notes.
