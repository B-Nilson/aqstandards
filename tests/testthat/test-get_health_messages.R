# Executable contract for get_health_messages -------------------------------
# Default (NULL categories) returns every category in `language`, in group
# order. Explicit categories are matched to the requested language, preserving
# input order; NA categories produce NA rows.
# Tests assert literal expectations only -- never package internals.
#
# Coverage matrix (consolidated when the file was table-ized):
#   - old default-output block (derived from internals) -> literal tables below
#   - old reorder/subset/factor/NA block                 -> "explicit categories"
#   - old tautological "output is unchanged" block       -> removed; subsumed by
#     the literal default tables below

# NOTE: expected_en/expected_fr below are the intentional full-content lock on
# the message data (package snapshots truncate long strings, so only literal
# text can catch a wrong word). Do not derive them from internal tables -- that
# would make the assertions tautological.

expected_en <- dplyr::tibble(
  risk_category = c("Low", "Moderate", "High", "Very High"),
  high_risk_pop_message = c(
    "Enjoy your usual activities.",
    "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
    "Reduce or reschedule activities outdoors.",
    "Avoid strenuous activity outdoors."
  ),
  general_pop_message = c(
    "Ideal air quality for outdoor activities.",
    "No need to modify your usual activities unless you experience symptoms.",
    "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
    "Reduce or reschedule activities outdoors, especially if you experience symptoms."
  )
)

expected_fr <- dplyr::tibble(
  risk_category = c("Faible", "Mod\u00e9r\u00e9", "Elev\u00e9", "Tr\u00e8s Elev\u00e9"),
  high_risk_pop_message = c(
    "Profitez des activit\u00e9s ext\u00e9rieures habituelles.",
    "Envisagez de r\u00e9duire ou de reporter les activit\u00e9s ext\u00e9rieures en plein air si vous \u00e9prouvez des sympt\u00f4mes.",
    "R\u00e9duisez ou r\u00e9organisez les activit\u00e9s ext\u00e9rieures en plein air.",
    " \u00c9vitez les activit\u00e9s ext\u00e9rieures en plein air."
  ),
  general_pop_message = c(
    "Qualit\u00e9 de l'air id\u00e9ale pour les activit\u00e9s en plein air.",
    "Aucun besoin de modifier vos activit\u00e9s habituelles en plein air \u00e0 moins d'\u00e9prouver des sympt\u00f4mes comme la toux ou une irritation de la gorge.",
    "Envisagez de r\u00e9duire ou de reporter les activit\u00e9s ext\u00e9rieures en plein air si vous \u00e9prouvez des sympt\u00f4mes.",
    "R\u00e9duisez ou r\u00e9organisez les activit\u00e9s ext\u00e9rieures en plein air, surtout si vous \u00e9prouvez des sympt\u00f4mes comme la toux ou une irritation de la gorge."
  )
)

test_that("default output returns the full English/French message tables", {
  expect_equal(get_health_messages(), expected_en)
  expect_equal(get_health_messages(language = "fr"), expected_fr)
  # The default adapts to the language (fr categories, not the en ones)
  expect_equal(get_health_messages(language = "fr")$risk_category,
               c("Faible", "Mod\u00e9r\u00e9", "Elev\u00e9", "Tr\u00e8s Elev\u00e9"))
})

test_that("explicit categories keep input order and NA stays NA", {
  # Reordered and subset requests follow the input, not the table
  expect_equal(
    get_health_messages(c("High", "Low"))$risk_category,
    c("High", "Low")
  )
  expect_equal(
    get_health_messages(c("Elev\u00e9", "Faible"), language = "fr")$risk_category,
    c("Elev\u00e9", "Faible")
  )

  # Factor and NA inputs are handled positionally
  factor_in <- c("Low", NA, "Very High") |>
    factor(levels = c("Low", "Moderate", "High", "Very High")) |>
    get_health_messages()
  expect_equal(factor_in$risk_category, c("Low", NA, "Very High"))

  all_na <- get_health_messages(rep(NA_character_, 2))
  expect_equal(nrow(all_na), 2)
  expect_true(all(is.na(all_na$risk_category)))
})
