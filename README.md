
# aqstandards

<!-- badges: start -->
<!-- badges: end -->

The goal of aqstandards is to provide an easy interface for assessing various air quality standards and indices.

## Development: internal architecture (AQHI/AQHI+)

For contributors touching the AQHI/AQHI+ scope, the level scale and its data
have a single owner, `R/aqhi_tables.R` (see the module map in its header):

- `R/aqhi_tables.R` - the level scale (1-10, "+"), risk-group membership,
  per-level colours, and en/fr risk labels and health messages, plus the
  read-only accessors `.aqhi_level_risk()`, `.aqhi_level_colours()`, and
  `.aqhi_risk_messages()`.
- `R/aqhi_plus.R` - AQHI+ engine: invalid-input policy and PM2.5 binning in
  the internal `aqhi_plus_map()`, and the exported `AQHI_plus()` renderer.
- `R/aqhi.R` - the 3-pollutant AQHI engine (rolling means, formula, AQHI+
  override) and its renderer.
- `R/get_risk_category.R`, `R/get_health_messages.R`, `R/get_aqhi_colours.R`
  - thin lookups over the tables (level -> risk, risk -> messages,
    level/PM2.5 -> colour). They own no data.

Data flows one way: tables -> engines -> lookups -> output. Engine internals
must be reused (e.g. `get_aqhi_colours(types = "pm25_1hr")` calls
`aqhi_plus_map()`), never the exported renderers.

## Installation

You can install the development version of aqstandards like so:

``` r
# install.packages("remotes")
remotes::install_github("B-Nilson/aqstandards")

```

## Canadian Ambient Air Quality Standards (CAAQS)

`CAAQS()` assesses hourly observations against the Canadian Ambient Air
Quality Standards: for each pollutant it derives the annual metric values
defined by the CCME guidance documents, averages them over 3-year windows,
and classifies the resulting metric values against the standards and the
management-level bands of the Air Zone Management guidance.

- **O3** — daily maximum of the 8-hour rolling mean (each window is
  attributed to the ending hour it labels, eq. 5.1 of the Ozone GDAD);
  annual metric = 4th-highest daily maximum over April 1 – September 30;
  standard 63 ppb (2015), 62 (2020), 60 (2025).
- **NO2** — annual metric = mean of hourly values, standard 17 ppb (2020),
  12 (2025); 1-hour metric = 98th percentile of daily maxima, 60 ppb
  (2020), 42 (2025).
- **SO2** — annual metric = mean of hourly values, standard 5 ppb (2020),
  4 (2025); 1-hour metric = 99th percentile of daily maxima, 70 ppb (2020),
  65 (2025).
- **PM2.5** — daily metric = 98th percentile of daily 24-hour means,
  standard 28 µg/m³ (2015), 27 (2020); annual metric = mean of daily means,
  10.0 µg/m³ (2015), 8.8 (2020).

Metric values are computed only for years (and 3-year windows) meeting the
data-completeness criteria of each guidance document's Table 5-3 — at least
18 of 24 valid hours per day, 75% of days in the year and 60% in every
calendar quarter, the April–September season for O3 — with the documents'
exceptions retaining deficient days or gated-out years whose values exceed
the standard. Percentiles use the GDAD ranking approach (K-th highest, no
interpolation), values are rounded per each GDAD before comparison with the
standards, and a 3-year metric value requires at least two of the three
annual values.

Sources: CCME Guidance Documents on Achievement Determination (Ozone 2021;
NO2 and SO2 2020; PM2.5 2012, PN 1483) and the Guidance Document on Air
Zone Management (2019, Appendix 2).

``` r
library(aqstandards)
hours <- seq(
  lubridate::ymd_h("2021-01-01 00"),
  lubridate::ymd_h("2023-12-31 23"), "1 hours"
)
obs <- data.frame(
  date = hours,
  pm25 = rep(10, length(hours)),
  o3 = rep(30, length(hours)),
  no2 = rep(10, length(hours)),
  so2 = rep(1, length(hours))
)
# a few elevated O3 plateau days each summer
for (day in paste0(rep(2021:2023, each = 4),
                   c("-06-10", "-06-20", "-07-10", "-07-20"))) {
  obs$o3[obs$date %in% seq(
    lubridate::ymd_h(paste(day, "09")),
    lubridate::ymd_h(paste(day, "16")), "1 hours"
  )] <- 70
}
CAAQS(
  dates = obs$date, 
  pm25_1hr_ugm3 = obs$pm25,
  o3_1hr_ppb = obs$o3, 
  no2_1hr_ppb = obs$no2, 
  so2_1hr_ppb = obs$so2
)
```

## Canadian Air Quality Health Index

<!-- TODO: add writeup on standard -->

``` r
library(aqstandards)
obs <- data.frame(
    date = seq(
        as.POSIXct("2024-01-01 00:00:00"),
        as.POSIXct("2024-01-01 23:00:00"),
        "1 hours"
    ),
    pm25 = 1:101,
    o3 = 1:101,
    no2 = 1:101
)

# AQHI
obs$date |> 
    AQHI(
        pm25_1hr_ugm3 = obs$pm25,
        o3_1hr_ppb = obs$o3,
        no2_1hr_ppb = obs$no2
    )

# AQHI+
obs$date |> AQHI(pm25_1hr_ugm3 = obs$pm25)
```

## U.S.A. Air Quality Index (AQI)

<!-- TODO: add writeup on standard -->

``` r
library(aqstandards)
AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
```
