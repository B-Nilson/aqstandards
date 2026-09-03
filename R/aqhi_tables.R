# =============================================================================
# AQHI / AQHI+ level scale -- single owner of shared level data
# =============================================================================
# Module map for the AQHI/AQHI+ scope:
#
#   R/aqhi_tables.R     data: level scale + risk groups (membership, colours,
#                       health messages en/fr) and the read-only accessors used
#                       by the engines and helpers below. Nothing here calls
#                       into the engines or helpers.
#
#   R/aqhi_plus.R       AQHI+ engine: 1-h PM2.5 validation/censoring + binning
#                       (aqhi_plus_map) and the detailed tibble renderer.
#   R/aqhi.R            AQHI engine: rolling means, formula, rounding and the
#                       AQHI+ override, plus the detailed tibble renderer.
#
#   R/get_risk_category.R  level -> risk category (en/fr)
#   R/get_health_messages.R  risk category -> health messages (en/fr)
#   R/get_aqhi_colours.R    level (or 1-h PM2.5) -> colour
#
# Data flow (one direction, no cycles):
#   aqhi_tables.R  ->  engines (levels only)  ->  helpers (labels/messages/
#                    colours)  ->  presentation tibbles
# The engines never own risk/message/colour data and the helpers never own
# level data; any change to the scale, breakpoints, categories, colours or
# messaging lands in exactly one place here (plus the AQHI+ PM2.5 bin width,
# which is a contract of the engine in R/aqhi_plus.R).
# =============================================================================

# The display level scale shared by the AQHI and AQHI+ (factor order).
.aqhi_levels <- c(1:10, "+")

# One row per health-risk group, in display order (Low -> Very High).
# `levels` holds the AQHI levels in that group; concatenated in row order it
# must equal .aqhi_levels. Colours are per level, messages per group/language.
.aqhi_risk_groups <- dplyr::tibble(
  risk_en = c("Low", "Moderate", "High", "Very High"),
  risk_fr = c(
    "Faible",
    "Mod\u00e9r\u00e9",
    "Elev\u00e9",
    "Tr\u00e8s Elev\u00e9"
  ),
  levels = list(1:3, 4:6, 7:10, "+"),
  colours = list(
    c("#21C6F5", "#189ACA", "#0D6797"),
    c("#FFFD37", "#FFCC2E", "#FE9A3F"),
    c("#FD6769", "#FF3B3B", "#FF0101", "#CB0713"),
    "#650205"
  ),
  high_risk_pop_message_en = c(
    "Enjoy your usual activities.",
    "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
    "Reduce or reschedule activities outdoors.",
    "Avoid strenuous activity outdoors."
  ),
  general_pop_message_en = c(
    "Ideal air quality for outdoor activities.",
    "No need to modify your usual activities unless you experience symptoms.",
    "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
    "Reduce or reschedule activities outdoors, especially if you experience symptoms."
  ),
  high_risk_pop_message_fr = c(
    "Profitez des activit\u00e9s ext\u00e9rieures habituelles.",
    "Envisagez de r\u00e9duire ou de reporter les activit\u00e9s ext\u00e9rieures en plein air si vous \u00e9prouvez des sympt\u00f4mes.",
    "R\u00e9duisez ou r\u00e9organisez les activit\u00e9s ext\u00e9rieures en plein air.",
    " \u00c9vitez les activit\u00e9s ext\u00e9rieures en plein air."
  ),
  general_pop_message_fr = c(
    "Qualit\u00e9 de l'air id\u00e9ale pour les activit\u00e9s en plein air.",
    "Aucun besoin de modifier vos activit\u00e9s habituelles en plein air \u00e0 moins d'\u00e9prouver des sympt\u00f4mes comme la toux ou une irritation de la gorge.",
    "Envisagez de r\u00e9duire ou de reporter les activit\u00e9s ext\u00e9rieures en plein air si vous \u00e9prouvez des sympt\u00f4mes.",
    "R\u00e9duisez ou r\u00e9organisez les activit\u00e9s ext\u00e9rieures en plein air, surtout si vous \u00e9prouvez des sympt\u00f4mes comme la toux ou une irritation de la gorge."
  )
)

# Colour used when no level is available (missing/invalid input).
.aqhi_missing_colour <- "#bbbbbb"

# --- Read-only accessors -----------------------------------------------------
# These keep the group table private to this module so consumers read only the
# shape they need (level-aligned or group-aligned) in .aqhi_levels order:
# .aqhi_level_risk(), .aqhi_level_colours(), .aqhi_risk_messages(),
# .aqhi_risk_labels(), and the shared .check_language() guard.

# Risk category label for every level, in .aqhi_levels order.
.aqhi_level_risk <- function(language = "en") {
  groups <- .aqhi_risk_groups
  label_col <- if (language == "en") "risk_en" else "risk_fr"
  rep(groups[[label_col]], lengths(groups$levels))
}

# Colour for every level, named by level (in .aqhi_levels order).
.aqhi_level_colours <- function() {
  colours <- unlist(.aqhi_risk_groups$colours, use.names = FALSE)
  stats::setNames(colours, unlist(.aqhi_risk_groups$levels))
}

# Health message table for one language, in group (Low -> Very High) order.
.aqhi_risk_messages <- function(language = "en") {
  groups <- .aqhi_risk_groups
  dplyr::tibble(
    risk_category = groups[[paste0("risk_", language)]],
    high_risk_pop_message = groups[[paste0("high_risk_pop_message_", language)]],
    general_pop_message = groups[[paste0("general_pop_message_", language)]]
  )
}

# Every valid risk-category label (both languages, then NA): the universe used
# to validate category input. Callers must not read .aqhi_risk_groups directly.
.aqhi_risk_labels <- function() {
  c(.aqhi_risk_groups$risk_en, .aqhi_risk_groups$risk_fr, NA)
}

# Shared language guard: validates `language` and returns it lowercased.
# Single owner of the en/fr language policy for the AQHI/AQHI+ scope.
.check_language <- function(language) {
  stopifnot(
    is.character(language),
    length(language) == 1,
    tolower(language) %in% c("en", "fr")
  )
  tolower(language)
}
