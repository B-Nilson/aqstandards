#' Get the risk category for each AQHI level
#'
#' @description
#' AQHI levels are broken down into categories for easier communication of health risks:
#'  - Low: AQHI = 1-3
#'  - Moderate: AQHI = 4-6
#'  - High: AQHI = 7-10
#'  - Very High: AQHI = "+" (>10)
#'
#' Health messages for at risk population and general population are available for each risk category - see \code{\link{get_health_messages}}
#'
#' @param aqhi_levels (Optional).
#'   A vector of AQHI levels (1-10, "+", or NA).
#'   Default is all unique levels.
#' @inheritParams AQHI
#'
#' @references Environment and Climate Change Canada: \url{https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html}
#' @return A factor of risk categories (Low, Moderate, High, Very High) representing each level of \code{aqhi_levels}.
#' @export
#' @examples
#' # Get risk categories for all AQHI levels
#' get_risk_category()
#'
#' # The same, but en Francais
#' get_risk_category(language = "fr")
#'
#' # Get risk categories for obervations
#' aqhi_levels <- sample(c(1:10, "+"), 50, replace = TRUE)
#' get_risk_category(aqhi_levels)
get_risk_category <- function(aqhi_levels = c(1:10, "+"), language = "en") {
  stopifnot(
    is.factor(aqhi_levels) |
      is.character(aqhi_levels) |
      is.numeric(aqhi_levels) |
      all(is.na(aqhi_levels)),
    length(aqhi_levels) > 0,
    as.character(aqhi_levels) %in% c(.aqhi_levels, NA)
  )
  language <- .check_language(language)

  # One risk category label per level, in level order, from the shared scale
  # table (R/aqhi_tables.R) -- the sole owner of risk-group membership.
  aqhi_levels |>
    factor(
      levels = .aqhi_levels,
      labels = .aqhi_level_risk(language)
    )
}
