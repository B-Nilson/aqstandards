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
  the function help and on the threshold table. Interpretation choices:
  hourly input timestamps are treated as labelling the start of the
  averaging hour (so hour-ending values map directly onto hourly rows);
  the GDADs' remaining completeness and rounding rules (daily 18-of-24
  gates, 75% annual / 60% quarterly gates, April-September season for
  O3, and the decimal-place reporting rules) are not yet implemented.
  The GDAD for PM2.5 was not reviewed in this pass.

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
