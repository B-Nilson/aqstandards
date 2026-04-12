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

# Vectorized rolling mean # TODO: replace with handyr::rolling once handyr is on CRAN
roll_mean <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0
) {
  rolling_sum <- x |>
    roll_sum(
      width = width,
      direction = direction,
      fill = fill,
      min_non_na = min_non_na,
      .include_counts = TRUE
    )
  n_non_missing <- attr(rolling_sum, "n_non_missing")
  n_non_missing <- ifelse(n_non_missing == 0, NA, n_non_missing)
  as.numeric(rolling_sum) / n_non_missing
}

# Vectorized rolling sum # TODO: remove once handyr is on CRAN
roll_sum <- function(
  x,
  width = 3,
  direction = "backward",
  fill = NULL,
  min_non_na = 0,
  .include_counts = FALSE
) {
  value_matrix <- x |>
    build_roll_matrix(
      width = width,
      direction = direction,
      fill = fill
    )
  n_non_missing <- width - rowSums(is.na(value_matrix))
  rolling_sum <- rowSums(value_matrix, na.rm = TRUE)
  rolling_sum[n_non_missing < min_non_na] <- NA

  if (.include_counts) {
    n_possible <- rep(width, length(x))
    n_possible[1:width] <- 1:width
    attr(rolling_sum, "n") <- n_possible
    attr(rolling_sum, "n_non_missing") <- n_non_missing
  }

  if (!is.null(fill)) {
    if (direction == "backward") {
      rolling_sum[1:(width - 1)] <- fill
    } else if (direction == "forward") {
      rolling_sum[(length(x) - width + 1):length(x)] <- fill
    }
  }

  return(rolling_sum)
}

# Make lag/lead matrix for rolling functions # TODO: remove once handyr is on CRAN
build_roll_matrix <- function(x, width = 3, direction = "backward", fill = NA) {
  fill <- ifelse(is.null(fill), NA, fill)
  value_matrix <- matrix(fill, nrow = length(x), ncol = width)
  for (i in 0:(width - 1)) {
    if (direction == "backward") {
      value_matrix[(i + 1):length(x), i + 1] <- x[1:(length(x) - i)]
    } else if (direction == "forward") {
      value_matrix[1:(length(x) - i), i + 1] <- x[(i + 1):length(x)]
    } else {
      # TODO: implement center
      stop("direction must be 'backward' or 'forward'")
    }
  }
  return(value_matrix)
}
