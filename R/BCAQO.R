BCAQO <- function(
  dates,
  pm25_ugm3 = NULL,
  pm10_ugm3 = NULL,
  o3_ppb = NULL,
  no2_ppb = NULL,
  so2_ppb = NULL,
  na.rm = FALSE
) {
  stopifnot(
    length(dates) > 0,
    !anyNA(dates),
    is.logical(na.rm),
    length(na.rm) == 1,
    !is.null(pm25_ugm3) |
      !is.null(pm10_ugm3) |
      !is.null(o3_ppb) |
      !is.null(no2_ppb) |
      !is.null(so2_ppb),
    is.null(pm25_ugm3) |
      (is.numeric(pm25_ugm3) & length(pm25_ugm3) == length(dates)),
    is.null(pm10_ugm3) |
      (is.numeric(pm10_ugm3) & length(pm10_ugm3) == length(dates)),
    is.null(o3_ppb) |
      (is.numeric(o3_ppb) & length(o3_ppb) == length(dates)),
    is.null(no2_ppb) |
      (is.numeric(no2_ppb) & length(no2_ppb) == length(dates)),
    is.null(so2_ppb) |
      (is.numeric(so2_ppb) & length(so2_ppb) == length(dates))
  )
  inputs <- combine_inputs(
    date = dates,
    pm25 = pm25_ugm3,
    pm10 = pm10_ugm3,
    o3 = o3_ppb,
    no2 = no2_ppb,
    so2 = so2_ppb
  ) |>
    tidyr::complete(
      date = range(.data$date) |>
        handyr::as_interval() |>
        seq(by = handyr::get_step(.data$date) |> format())
    )

  list(
    bcaqo_pm(
      dates = inputs$date,
      pm25 = inputs$pm25,
      pm10 = inputs$pm10,
      na.rm = na.rm
    ),
    bcaqo_o3(
      dates = inputs$date,
      o3 = inputs$o3,
      na.rm = na.rm
    ),
    bcaqo_no2_so2(
      dates = inputs$date,
      no2 = inputs$no2,
      so2 = inputs$so2,
      na.rm = na.rm
    )
  ) |>
    handyr::join_list() |>
    dplyr::mutate(
      year = .data$date |> lubridate::year(),
      .after = "date"
    ) |>
    dplyr::select(-date) |>
    tidyr::nest(attainment = dplyr::starts_with("meets_aqo_"), ) |>
    dplyr::mutate(
      attainment = .data$attainment |>
        lapply(\(x) {
          new_names <- names(x) |> sub(pattern = "meets_aqo_", replacement = "")
          x |> stats::setNames(new_names)
        }) |>
        dplyr::bind_rows()
    )
}

bcaqo_pm <- function(dates, pm25 = NULL, pm10 = NULL, na.rm = FALSE) {
  thresholds <- list(
    pm25 = c(pm25_daily_mean_annual_98th = 25, pm25_daily_mean_annual_mean = 8),
    pm10 = c(pm10_daily_mean_annual_mean = 50)
  )
  pollutants <- names(thresholds)
  mean_cols <- paste0(pollutants, "_daily_mean")
  dailies <- combine_inputs(date = dates, pm25 = pm25, pm10 = pm10) |>
    get_daily_means(cols = pollutants, na.rm = na.rm)
  dailies |>
    get_annual_percentiles(
      cols = mean_cols[pollutants == "pm25"],
      percentiles = 98,
      na.rm = na.rm
    ) |>
    dplyr::full_join(
      dailies |> get_annual_means(cols = mean_cols, na.rm = na.rm),
      by = "date"
    ) |>
    check_threshold(thresholds = thresholds$pm25) |>
    check_threshold(thresholds = thresholds$pm10)
}

bcaqo_o3 <- function(dates, o3 = NULL, na.rm = FALSE) {
  thresholds <- c(o3_8hr_mean_daily_max_annual_4th_highest = 60)
  combine_inputs(date = dates, o3 = o3) |>
    get_8hr_means(cols = "o3", na.rm = na.rm) |>
    get_daily_maxima(cols = "o3_8hr_mean", na.rm = na.rm) |>
    get_annual_4th_highest(cols = "o3_8hr_mean_daily_max") |>
    check_threshold(thresholds = thresholds)
}

bcaqo_no2_so2 <- function(dates, no2 = NULL, so2 = NULL, na.rm = FALSE) {
  thresholds <- list(
    no2 = c(no2_daily_max_annual_98th = 60, no2_annual_mean = 17),
    so2 = c(so2_daily_max_annual_99th = 65, so2_annual_mean = 4)
  )
  inputs <- combine_inputs(date = dates, no2 = no2, so2 = so2)
  pollutants <- c("no2", "so2")
  inputs |>
    get_daily_maxima(cols = pollutants, na.rm = na.rm) |>
    get_annual_percentiles(
      cols = pollutants |> paste0("_daily_max"),
      percentiles = c(98, 99),
      na.rm = na.rm
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "no2_daily_max_annual_99th",
        "so2_daily_max_annual_98th"
      ))
    ) |>
    dplyr::full_join(
      inputs |> get_annual_means(cols = pollutants, na.rm = na.rm),
      by = "date"
    ) |>
    check_threshold(thresholds = thresholds$no2, years = c(3, 1)) |>
    check_threshold(thresholds = thresholds$so2, years = c(3, 1))
}
