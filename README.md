
# aqstandards

<!-- badges: start -->
<!-- badges: end -->

The goal of aqstandards is to provide an easy interface for assessing various air quality standards.

## Installation

You can install the development version of aqstandards like so:

``` r
# install.packages("remotes")
remotes::install_github("B-Nilson/aqstandards")

```

## Canadian Ambient Air Quality Standards (CAAQS)

<!-- TODO: add writeup on standard -->

``` r
library(aqstandards)
obs <- data.frame(
  date = seq(
    lubridate::ymd_h("2020-01-01 00"),
    lubridate::ymd_h("2023-12-31 23"), "1 hours"
  ),
  pm25 = sample(1:150, 35064, TRUE), o3 = sample(1:150, 35064, TRUE),
  no2 = sample(1:150, 35064, TRUE), so2 = sample(1:150, 35064, TRUE)
)
CAAQS(
  dates = obs$date, 
  pm25_1hr_ugm3 = obs$pm25,
  o3_1hr_ppb = obs$o3, 
  no2_1hr_ppb = obs$no2, 
  so2_1hr_ppb = obs$so2
)
```

## U.S.A. Air Quality Index (AQI)

<!-- TODO: add writeup on standard -->

``` r
library(aqstandards)
AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
```
