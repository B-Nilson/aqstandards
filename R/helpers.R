combine_inputs <- function(...) {
  inputs <- list(...)
  inputs |>
    purrr::compact() |>
    as.data.frame() |>
    dplyr::tibble()
}

get_8hr_means <- function(obs, cols, date_col = "date", na.rm = FALSE) {
  obs |>
    dplyr::group_by(
      date = lubridate::floor_date(.data[[date_col]], "8 hours")
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(cols),
        list("8hr_mean" = \(x) mean(x, na.rm = na.rm))
      ),
      .groups = "drop"
    )
}

get_daily_means <- function(obs, cols, date_col = "date", na.rm = FALSE) {
  obs |>
    dplyr::group_by(date = lubridate::floor_date(.data[[date_col]], "days")) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(cols),
        list(daily_mean = \(x) mean(x, na.rm = na.rm))
      ),
      .groups = "drop"
    )
}

get_daily_maxima <- function(obs, cols, date_col = "date", na.rm = FALSE) {
  obs |>
    dplyr::group_by(date = lubridate::floor_date(.data[[date_col]], "days")) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(cols),
        list(daily_max = \(x) handyr::max(x, na.rm = na.rm))
      ),
      .groups = "drop"
    )
}

get_annual_means <- function(obs, cols, date_col = "date", na.rm = FALSE) {
  obs |>
    dplyr::group_by(date = lubridate::floor_date(.data[[date_col]], "years")) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(cols),
        list(annual_mean = \(x) mean(x, na.rm = na.rm))
      ),
      .groups = "drop"
    )
}

get_annual_percentiles <- function(
  obs,
  cols,
  date_col = "date",
  percentiles = c(0:100)[1],
  na.rm = FALSE
) {
  stopifnot(
    is.numeric(percentiles),
    !anyNA(percentiles),
    length(percentiles) > 0,
    all(percentiles >= 0 & percentiles <= 100)
  )
  funs <- percentiles |>
    stats::setNames(paste0("annual_", percentiles, "th")) |>
    lapply(\(p) \(x) stats::quantile(x, probs = p / 100, na.rm = na.rm))

  obs |>
    dplyr::group_by(date = lubridate::floor_date(.data[[date_col]], "years")) |>
    dplyr::summarise(dplyr::across(dplyr::any_of(cols), funs), .groups = "drop")
}

get_annual_4th_highest <- function(x, cols, date_col = "date") {
  fun <- \(x) {
    i <- pmin(4, sum(!is.na(x)))
    if (i < 4) {
      cli::cli_warn(
        "Not enough non-missing data to calculate annual 4th highest, returning #{i} instead."
      )
    }
    sort(x, decreasing = TRUE)[i]
  }

  x |>
    dplyr::group_by(date = lubridate::floor_date(.data[[date_col]], "years")) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(cols), list("annual_4th_highest" = fun)),
      .groups = "drop"
    )
}

#' @importFrom rlang :=
check_threshold <- function(summary, thresholds, years = 1, na.rm = FALSE) {
  if (length(years) == 1) {
    years <- rep(years, length(thresholds))
  }
  new_col_names <- paste0("meets_aqo_", names(thresholds))
  fun <- \(values) {
    i <- which(names(thresholds) == dplyr::cur_column())
    exceeds <- seq_len(years[i]) |>
      stats::setNames(paste0("lag_", seq_len(years[i]))) |>
      lapply(\(lag) dplyr::lag(unname(values), lag - 1) > thresholds[i]) |>
      as.data.frame()
    is_failed <- rowSums(exceeds, na.rm = TRUE) > 0
    if (!all(is_failed)) {
      x <- as.data.frame(exceeds[which(!is_failed), ])
      is_failed[!is_failed] <- rowSums(x, na.rm = na.rm) > 0
    }
    return(is_failed)
  }
  summary |>
    dplyr::mutate(
      dplyr::across(names(thresholds) |> stats::setNames(new_col_names), fun)
    )
}
