# Single owner of the input-handling scenarios (issue #4 matrix gaps:
# non-contiguous input dates, explicit NA inputs, and date/length
# validation). Both
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

  # Duplicated timestamps would silently double-count hours as extra
  # observations in every metric (probed on the pre-guard code: a
  # duplicated month at 9 ppb beside a 5 ppb background reported an
  # annual mean of 5.3 and a distorted 98th percentile, with no warning),
  # and NA or empty `dates` crashed in the calendar machinery with opaque
  # internal errors. The GDADs assume a unique hourly record per
  # timestamp, so CAAQS() guards both at its only entry point; the
  # conditions are asserted in test-CAAQS-inputs.R and the generator
  # wraps this in tryCatch() for the document.
  duplicated_timestamps = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    CAAQS(dates = c(hours, hours[1]), pm25_1hr_ugm3 = rep(1, length(hours) + 1))
  }),

  na_or_empty_timestamps = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    hours[[5]] <- as.POSIXct(NA)
    CAAQS(dates = hours, pm25_1hr_ugm3 = rep(1, length(hours)))
  }),

  # Non-datetime input (character strings, Date-class days) previously
  # surfaced either a lubridate class error from internal machinery or a
  # misleading "duplicated hours" error; the POSIXct class guard gives it
  # an explicit contract, mirroring AQHI().
  non_datetime_dates = quote({
    hours <- make_hours("2021-01-01 00", "2023-12-31 23")
    CAAQS(dates = as.character(hours), pm25_1hr_ugm3 = rep(1, length(hours)))
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
