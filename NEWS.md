# aqstandards 0.1.0

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
