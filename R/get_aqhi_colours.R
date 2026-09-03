#' Get AQHI colours for each AQHI level
#'
#' @description
#' Every AQHI level (1-10, "+", and NA) is assigned a hexidecimal colour code for visualization.
#'
#' These colours are defined by \href{https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html}{Environment and Climate Change Canada} such that:
#' - Low AQHI (1-3) are light to dark blue
#' - Moderate AQHI (4-6) are yellow to orange
#' - High AQHI (7-10) are light to dark red
#' - Very High AQHI (+) is darker red
#' - Missing AQHI (NA) is light grey
#'
#' @param values  (Optional).
#'   A vector of AQHI levels (1-10, "+", or NA) if `type` is "aqhi" OR
#'   A vector of hourly PM2.5 concentrations (ug/m3) if `type` is "pm25_1hr".
#'   Default is all AQHI levels.
#' @param types (Optional).
#'   A single character value, or a vector the same length as `values`, indicating the type(s) of values in `values`.
#'   Must all be within either "aqhi" or "pm25_1hr".
#'   Default is "aqhi".
#'
#' @references Environment and Climate Change Canada: \url{https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html}
#' @return A character vector of hexidecimal codes correspoding to each level of \code{aqhi_levels}.
#' @export
#' @examples
#' # Get AQHI colours for all AQHI levels
#' get_aqhi_colours()
#'
#' # Get AQHI colours for observation data
#' hourly_pm25_ugm3 <- sample(1:100, 50, replace = TRUE)
#' aqhi_levels <- hourly_pm25_ugm3 |>
#'   AQHI_plus(detailed = FALSE)
#' aqhi_levels |> get_aqhi_colours()
#'
#' # The same but with PM2.5 provided
#' hourly_pm25_ugm3 |> get_aqhi_colours(types = "pm25_1hr")
#'
#' # Or even a mix
#' values <- c(aqhi_levels, hourly_pm25_ugm3)
#' types <-  rep("aqhi", length(aqhi_levels)) |>
#'   c(rep("pm25_1hr", length(hourly_pm25_ugm3)))
#'
#' values |> get_aqhi_colours(types = types)
#'
get_aqhi_colours <- function(values = c(1:10, "+", NA), types = "aqhi") {
  stopifnot(
    is.factor(values) |
      is.character(values) |
      is.numeric(values) |
      all(is.na(values)),
    length(values) > 0
  )
  stopifnot(
    is.character(types),
    length(types) == 1 | length(types) == length(values),
    all(tolower(types) %in% c("aqhi", "pm25_1hr"))
  )

  # Convert type to lowercase and ensure right length
  types <- tolower(types)
  if (length(types) == 1) {
    types <- rep(types, length(values))
  }

  # Generate aqhi level labels from pm25 using the shared AQHI+ engine
  # (not the exported renderer, which would re-validate and rebuild a tibble)
  is_pm25 <- types %in% "pm25_1hr"
  if (any(is_pm25)) {
    values[is_pm25] <- aqhi_plus_map(values[is_pm25])$level |>
      as.character()
  }

  # Normalize everything to level labels, mapping the legacy numeric "11"
  # spelling of 10+ to "+"
  level_labels <- as.character(values)
  level_labels[!is_pm25 & level_labels == "11"] <- "+"

  # Look up colours by level; anything without a level (NA, invalid) is grey
  colours <- .aqhi_level_colours()[level_labels]
  colours[is.na(colours)] <- .aqhi_missing_colour
  unname(colours)
}
