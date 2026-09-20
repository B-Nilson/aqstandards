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
