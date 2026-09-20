## CAAQS configuration: the standards/management-level thresholds and the
## CAAQS Completeness -----------------------------------------------------
## Data-completeness configuration for each pollutant, one row per criterion,
## quoting the Data completeness criteria (column 2) of Table 5-3 of the CCME
## Guidance Document on Achievement Determination for each pollutant (Ozone
## 2021; Nitrogen Dioxide 2020; Sulphur Dioxide 2020; Fine Particulate Matter
## and Ozone 2012, PN 1483, sections 4.1.4 and 4.2.4 for PM2.5).
##
## Column 3 of the same tables lists the exceptions to those criteria; this
## configuration is also the single owner of which of those exceptions apply
## per pollutant, alongside the criteria they qualify:
##
##   * Daily-row exception ("The [O3 Dmax 8-hour / NO2 Dmax 1-hour / SO2 Dmax
##     1-hour] exceeds the standard"): a deficient day is retained when its
##     value exceeds the standard. Implemented in CAAQS_o3/no2/so2.
##
##   * Annual percentile-row exception (O3: "The annual fourth highest
##     exceeds the standard"; NO2: "The 98th percentile based on the
##     available NO2 Dmax 1-hour exceeds the standard"; SO2: "The 99th
##     percentile based on the available SO2 Dmax 1-hour exceeds the
##     standard"): a year failing the annual days/quarters criteria still
##     contributes its percentile-type metric when that metric, computed on
##     all available data of the year, exceeds the standard. Implemented in
##     the pipelines via `annual_metric_exception`.
##
##   * NO2/SO2 annual-metric-value exception ("1. at least 50% of the [NO2 /
##     SO2] 1-hour are available in each calendar quarter; and 2. the annual
##     average exceeds the standard"): the annual-mean metric of a year
##     failing the 75%-of-hours / 60%-per-quarter criteria is still computed
##     under the relaxed 50%-per-calendar-quarter criterion when the annual
##     average exceeds the standard. Implemented in the pipelines via
##     `annual_mean_exception`.
##
##   * PM2.5's 2012 GDAD predates the Table 5-3 format and has no
##     data-completeness exceptions column at all (its "exceptional events"
##     procedures in section 7 are an administrative TF/EE designation
##     process, not a data-completeness rule); none are implemented for
##     PM2.5.
CAAQS_completeness <- function() {
  list(
    # Ozone GDAD (2021), Table 5-3 (Ozone Dmax 8-hour): "O3 Dmax 8-hour are
    # available for at least 75% of the days in the period April 1 to
    # September 30." The annual fourth-highest ranking is restricted to that
    # season because it is "the time of year where the annual fourth highest
    # will likely be recorded in most of Canada" (GDAD section 5.3). The
    # annual row's column-3 exception ("The annual fourth highest exceeds
    # the standard") retains a year that fails the season days criterion
    # when its annual fourth highest, computed on all available data of the
    # year, exceeds the standard (annual_metric_exception below).
    o3 = list(
      min_hours_of_day = 18L,
      season = c(start = "04-01", end = "09-30"),
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = NULL,
      min_hours_fraction_year = NULL,
      min_hours_fraction_quarters = NULL,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE
    ),
    # Nitrogen Dioxide GDAD (2020), Table 5-3:
    #   NO2 Dmax 1-hour: "At least 18 of the 24 (75%) NO2 1-hour are
    #     available in the day."
    #   Annual 98th percentile of the NO2 Dmax 1-hour: "The NO2 Dmax 1-hour
    #     are available for at least: 1. 75% of the days in a year; and
    #     2. 60% of the days in each calendar quarter."
    #   Annual metric value: "1. at least 75% of the NO2 1-hour are available
    #     in the year; and 2. at least 60% of the NO2 1-hour are available in
    #     each calendar quarter."
    # Exceptions: the percentile row's exception ("The 98th percentile based
    # on the available NO2 Dmax 1-hour exceeds the standard") retains a year
    # failing the days/quarters criteria when its 98th percentile, computed
    # on all available data of the year, exceeds the standard
    # (annual_metric_exception below). The annual metric value row's
    # exception ("1. at least 50% of the NO2 1-hour are available in each
    # calendar quarter; and 2. the annual average exceeds the standard")
    # computes the annual-mean metric under the relaxed 50%-per-quarter
    # criterion when the annual average exceeds the standard
    # (annual_mean_exception below).
    # Calendar quarters (GDAD footnote): Q1 January 1 - March 31; Q2 April 1
    # - June 30; Q3 July 1 - September 30; Q4 October 1 - December 31.
    no2 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = 0.75,
      min_hours_fraction_quarters = 0.6,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE,
      annual_mean_exception = TRUE
    ),
    # Sulphur Dioxide GDAD (2020), Table 5-3: identical structure to NO2 with
    # SO2 in place of NO2 (SO2 Dmax 1-hour "At least 18 of the 24 (75%) ...
    # available in the day"; annual 99th percentile "75% of the days in a
    # year and 60% of the days in each calendar quarter"; annual metric
    # value "75% of the SO2 1-hour ... in the year and 60% ... in each
    # calendar quarter"). Exceptions: as for NO2, the daily-row exception
    # ("The SO2 Dmax 1-hour exceeds the standard") is implemented in
    # CAAQS_so2(), the percentile-row exception ("The 99th percentile based
    # on the available SO2 Dmax 1-hour exceeds the standard") via
    # annual_metric_exception, and the annual metric value row's exception
    # ("at least 50% ... in each calendar quarter; and 2. the annual average
    # exceeds the standard") via annual_mean_exception.
    so2 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = 0.75,
      min_hours_fraction_quarters = 0.6,
      min_days_of_quarters = NULL,
      annual_metric_exception = TRUE,
      annual_mean_exception = TRUE
    ),
    # PM2.5 Guidance Document on Achievement Determination (PN 1483, "Fine
    # Particulate Matter and Ozone", 2012), sections 4.1.1, 4.1.4 and 4.2.4:
    # the daily 24hr-PM2.5 is valid when "at least 75% (18 hours) of the
    # 1-hour concentrations are available on the given day", and both the
    # annual 98P and the annual average require "at least 75% valid daily-
    # 24hr-PM2.5 in the year" and "at least 60% valid daily-24hr-PM2.5 in
    # each calendar quarter" (quarters Q1 January 1 - March 31 through Q4
    # October 1 - December 31). Unlike NO2/SO2 there is no hours-per-year
    # criterion: the annual gates are expressed in valid days only, and the
    # 2012 document has no exceptions column (see the header comment above).
    pm25 = list(
      min_hours_of_day = 18L,
      season = NULL,
      min_days_fraction = 0.75,
      min_days_of_year = NULL,
      min_days_fraction_quarters = 0.6,
      min_hours_fraction_year = NULL,
      min_hours_fraction_quarters = NULL,
      min_days_of_quarters = NULL,
      annual_metric_exception = FALSE,
      annual_mean_exception = FALSE
    )
  )
}
# Current as of 2025-08 (CCME air quality report, https://ccme.ca/en/air-quality-report).
# Threshold values are the management-level band edges of the CCME Guidance
# Document on Air Zone Management (2019), Appendix 2 (Tables A2-1 to A2-4),
# with the effective year as the list name: Red is the CAAQS itself, the
# Orange and Yellow values are the inclusive lower edges shown there ("32 to
# 60 ppb", "21 to 31 ppb", ...), and Green is everything below the Yellow
# lower edge ("< 50 ppb" in the tables). CAAQS_meets_standard() applies the
# comparison semantics the guidance documents state: a CAAQS "is achieved if
# the metric value is less than or equal to the standard" (achievement
# determination GDADs), i.e. Red is strict >, and the band lower edges are
# inclusive. Metric values are rounded per the GDADs (CAAQS_round_gdad())
# before comparison, as Appendix 2 requires ("the metric values for
# comparison to the concentrations must be rounded to the same number of
# digits as the shown concentrations"). The CAAQS O3 metric is defined in the CCME Guidance
# Document on Achievement Determination for Ozone (2021): 3-year average of the
# annual 4th-highest daily maximum 8-hour rolling average. The CAAQS PM2.5
# metrics are defined in the CCME Guidance Document on Achievement
# Determination for Fine Particulate Matter and Ozone (2012, PN 1483),
# sections 4.1 and 4.2: 3-year averages of the annual 98th percentile of
# daily 24-hr means (rank-percentile) and of annual averages of valid daily
# 24-hr means.
CAAQS_thresholds <- function() {
  list(
    pm25 = list(
      daily = list(
        "2015" = c(Red = 28, Orange = 20, Yellow = 11, Green = 0),
        "2020" = c(Red = 27, Orange = 20, Yellow = 11, Green = 0)
      ),
      annual = list(
        "2015" = c(Red = 10, Orange = 6.5, Yellow = 4.1, Green = 0),
        "2020" = c(Red = 8.8, Orange = 6.5, Yellow = 4.1, Green = 0)
      )
    ),
    o3 = list(
      `8hr` = list(
        "2015" = c(Red = 63, Orange = 57, Yellow = 51, Green = 0),
        "2020" = c(Red = 62, Orange = 57, Yellow = 51, Green = 0),
        "2025" = c(Red = 60, Orange = 57, Yellow = 51, Green = 0)
      )
    ),
    no2 = list(
      hourly = list(
        "2020" = c(Red = 60, Orange = 32, Yellow = 21, Green = 0),
        "2025" = c(Red = 42, Orange = 32, Yellow = 21, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 17, Orange = 7.1, Yellow = 2.1, Green = 0),
        "2025" = c(Red = 12, Orange = 7.1, Yellow = 2.1, Green = 0)
      )
    ),
    so2 = list(
      hourly = list(
        "2020" = c(Red = 70, Orange = 51, Yellow = 31, Green = 0),
        "2025" = c(Red = 65, Orange = 51, Yellow = 31, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 5, Orange = 3.1, Yellow = 2.1, Green = 0),
        "2025" = c(Red = 4, Orange = 3.1, Yellow = 2.1, Green = 0)
      )
    )
  )
}
# TODO: implement
CAAQS_objectives <- function(mgmt_levels) {
  c(
    Green = "To maintain good air quality through proactive air management measures to keep clean areas clean.",
    Yellow = "To improve air quality using early and ongoing actions for continuous improvement.",
    Orange = "To improve air quality through active air management and prevent exceedance of the CAAQS.",
    Red = "To reduce pollutant levels below the CAAQS through advanced air management actions."
  )
}
