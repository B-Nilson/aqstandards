# Executable contract for get_risk_category ---------------------------------
# Levels 1-3 -> Low/Faible, 4-6 -> Moderate/Modere, 7-10 -> High/Eleve,
# "+" -> Very High/Tres Eleve. Output is a factor with all four categories.
# Tests assert literal expectations only -- never package internals.
#
# Coverage matrix (consolidated when the file was table-ized):
#   - old input-shape block (default, NA, numeric, "+", factor) -> "any level-
#     shaped input"
#   - old French + case-insensitivity block                     -> "every level"
#   - old weak `levels(get_risk_category())` assertion          -> folded into
#     the literal factor expectations below
# NOTE: expected_en/expected_fr are the literal lock on the risk labels; keep
# them hand-written rather than derived from the canonical tables.

scale <- c(1:10, "+")
en_cats <- c("Low", "Moderate", "High", "Very High")
fr_cats <- c("Faible", "Mod\u00e9r\u00e9", "Elev\u00e9", "Tr\u00e8s Elev\u00e9")

# Risk label for every level, in scale order
expected_en <- c(rep("Low", 3), rep("Moderate", 3), rep("High", 4), "Very High")
expected_fr <- c(rep("Faible", 3), rep("Mod\u00e9r\u00e9", 3),
                 rep("Elev\u00e9", 4), "Tr\u00e8s Elev\u00e9")

test_that("every level maps to its category (en and fr)", {
  expect_equal(get_risk_category(), factor(expected_en, levels = en_cats))
  expect_equal(get_risk_category(language = "fr"), factor(expected_fr, levels = fr_cats))
  expect_equal(
    get_risk_category(scale, language = "FR"),
    get_risk_category(scale, language = "fr")
  )
})

test_that("any level-shaped input maps consistently and NA stays NA", {
  expected_en_f <- factor(expected_en, levels = en_cats)
  expect_equal(get_risk_category(scale), expected_en_f)
  expect_equal(get_risk_category(1:10), expected_en_f[1:10])
  expect_equal(get_risk_category("+"), expected_en_f[11])
  # Factor input with full scale levels behaves like character input
  expect_equal(get_risk_category(factor(scale, levels = scale)), expected_en_f)
  # NA anywhere stays NA without shifting positions
  with_na <- c(NA, scale)
  expect_equal(
    get_risk_category(with_na),
    factor(c(NA, expected_en), levels = en_cats)
  )
  expect_equal(
    get_risk_category(rep(NA_character_, 3)),
    factor(rep(NA_character_, 3), levels = en_cats)
  )
})
