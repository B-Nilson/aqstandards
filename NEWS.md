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
  The GDADs' remaining rounding rules (decimal places and rounding of
  metric values before comparison) and the PM2.5 GDAD are not yet
  covered; see the completeness section below.
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
  The Table 5-3 exceptions criteria (values that exceed the standard are
  retained despite missing data) are not implemented: deficient days are
  dropped, which can only lower a metric value.
- `min_completeness` now applies only to PM2.5, whose guidance document
  has not been reviewed; it is ignored for O3, NO2 and SO2, whose gates
  follow the guidance documents. This is a user-visible behaviour
  change: years accepted (or rejected) by the old 50% heuristic may now
  be rejected (or accepted) where the Table 5-3 criteria differ.
- Hours-per-year requirements are derived from the calendar via
  lubridate's leap-year rule instead of the previously hardcoded
  `year %% 4` check, which mishandled century years such as 2100.
- Fixed an error when the data span non-consecutive years: a year absent
  from the data (for example 2022 in a 2021-2024 series) no longer
  propagates NA through the three-consecutive-years completeness check.

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
