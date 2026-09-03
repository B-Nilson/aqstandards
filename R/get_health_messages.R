#' Get health messages for AQHI+ risk categories
#'
#' @description
#'
#' Health messages for at risk population and general population are available for each AQHI health risk category (see \code{\link{get_risk_category}}).
#'
#' English messages returned by this function:
#'
#' | Risk | AQHI | At Risk Population | General Population |
#' | --- | --- | --- | --- |
#' | Low | 1 - 3 | Enjoy your usual activities. | Ideal air quality for outdoor activities. |
#' | Moderate | 4 - 6 | Consider reducing or rescheduling activities outdoors if you experience symptoms. | No need to modify your usual activities unless you experience symptoms. |
#' | High | 7 - 10 | Reduce or reschedule activities outdoors. | Consider reducing or rescheduling activities outdoors if you experience symptoms. |
#' | Very High | >10 | Avoid strenuous activity outdoors. | Reduce or reschedule activities outdoors, especially if you experience symptoms. |
#'
#' See \href{https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html}{Environment and Climate Change Canada's website} for more information.
#'
#' @param risk_categories (Optional).
#'   A factor or character vector of AQHI risk categories (e.g. Low, Moderate, High, Very High).
#'   If NULL (the default) all risk categories are returned, in `language`.
#' @inheritParams AQHI
#'
#' @return A tibble of health messages (for at risk population and general population) for the provided risk categories.
#' @export
#' @examples
#' # Get health messages for all risk categories
#' get_health_messages()
#'
#' # The same, but en Francais
#' get_health_messages(language = "fr")
#'
#' # Get health messages for some observations
#' hourly_pm25_ugm3 <- sample(1:100, 50, replace = TRUE)
#' risk_categories <- hourly_pm25_ugm3 |>
#'   AQHI_plus(detailed = FALSE) |>
#'   get_risk_category()
#' risk_categories |> get_health_messages()
get_health_messages <- function(
  risk_categories = NULL,
  language = "en"
) {
  language <- .check_language(language)
  aqhi_messaging <- .aqhi_risk_messages(language)

  # Default to every risk category in the requested language
  if (is.null(risk_categories)) {
    return(aqhi_messaging)
  }

  # Categories must be valid labels in either language; NA is allowed
  stopifnot(
    length(risk_categories) > 0,
    is.factor(risk_categories) |
      is.character(risk_categories) |
      all(is.na(risk_categories)),
    all(as.character(risk_categories) %in% .aqhi_risk_labels())
  )

  # Match risk categories to messages (preserving input order; NA stays NA)
  rows <- risk_categories |> match(aqhi_messaging$risk_category)
  aqhi_messaging[rows, ]
}
