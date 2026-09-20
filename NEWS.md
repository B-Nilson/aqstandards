# aqstandards 0.1.0

## CAAQS

Audit of `CAAQS()` against current CCME guidance (issue #3). The
standards table (Red thresholds and effective years: O3 63/62/60 ppb,
PM2.5 28/27 ug/m3 daily and 10/8.8 ug/m3 annual, NO2 60/42 ppb hourly
and 17/12 ppb annual, SO2 70/65 ppb hourly and 5/4 ppb annual for
2015/2020/2025 as applicable) is confirmed current as of 2025-08.

- The O3 8-hour metric now uses rolling 8-hour means starting at each
  hour (local standard time), per the CCME Guidance Document on
  Achievement Determination for Ozone (2021), instead of three
  non-overlapping 8-hour blocks per day. Daily maxima no longer miss
  peaks straddling block boundaries. Each rolling window requires at
  least 6 of 8 valid hours (documented assumption pending confirmation
  against the guidance PDF); windows are attributed to the day of their
  starting hour.
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
  the function help and on the threshold table.

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
