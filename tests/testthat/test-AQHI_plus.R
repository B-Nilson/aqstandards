# Executable contract for AQHI_plus ---------------------------------------
# Levels: right-closed 10 ug/m3 bins, a boundary belongs to the level starting
# at that value. Risk: Low 1-3 (<=30), Moderate 4-6 (>30-60), High 7-10
# (>60-100), Very High "+" (>100). Invalid/missing input -> NA, never shifted.
# Tests assert literal expectations only -- never package internals.
#
# Coverage matrix (cases consolidated when the file was table-ized):
#   - "levels deliberate at health boundaries" block      -> boundary_cases
#   - en/fr risk per health boundary block                -> boundary_cases + snapshots
#   - "0 and low positives" block                        -> boundary_cases rows (0, 0.001, 5)
#   - "'+' not 11", both-paths agreement, level order    -> "factor contract"
#   - general-pop message NA on invalid input (old block) -> staircase snapshot NA rows

# Case table: pm25 (1-h mean, ug/m3) -> level -> risk en/fr
boundary_cases <- data.frame(
  pm25 = c(0, 0.001, 5, 29.9, 30, 30.1, 59.9, 60, 60.1, 99.9, 100, 100.1),
  level = c("1", "1", "1", "3", "3", "4", "6", "6", "7", "10", "10", "+"),
  risk_en = c("Low", "Low", "Low", "Low", "Low", "Moderate", "Moderate",
              "Moderate", "High", "High", "High", "Very High"),
  risk_fr = c("Faible", "Faible", "Faible", "Faible", "Faible",
              "Mod\u00e9r\u00e9", "Mod\u00e9r\u00e9", "Mod\u00e9r\u00e9",
              "Elev\u00e9", "Elev\u00e9", "Elev\u00e9", "Tr\u00e8s Elev\u00e9")
)

test_that("AQHI+ maps 1-h PM2.5 to deliberate levels and risks (en/fr)", {
  expected_level <- factor(boundary_cases$level, levels = c(1:10, "+"))
  expected_risk_en <- factor(boundary_cases$risk_en,
                             levels = c("Low", "Moderate", "High", "Very High"))
  expected_risk_fr <- factor(boundary_cases$risk_fr,
                             levels = c("Faible", "Mod\u00e9r\u00e9", "Elev\u00e9",
                                        "Tr\u00e8s Elev\u00e9"))

  # Level-only and detailed paths agree and both languages are stable
  expect_equal(AQHI_plus(boundary_cases$pm25, detailed = FALSE), expected_level)
  expect_equal(AQHI_plus(boundary_cases$pm25)$level, expected_level)
  expect_equal(AQHI_plus(boundary_cases$pm25)$risk, expected_risk_en)
  expect_equal(AQHI_plus(boundary_cases$pm25, language = "fr")$risk, expected_risk_fr)

  # language is not case sensitive
  expect_equal(
    AQHI_plus(30.1, language = "FR")$risk,
    AQHI_plus(30.1, language = "fr")$risk
  )
})

test_that("AQHI+ returns expected output", {
  # Every 10 ug/m3 bin boundary, plus the lower/upper tails
  pm25_hourly <- c(NA, -1, 0:10 * 10, 0:10 * 10 + 0.1) |> sort(na.last = FALSE)
  expected_aqhi <- c(NA, NA, 1, rep(1:10, each = 2), "+") |>
    factor(levels = c(1:10, "+"))

  expect_snapshot(AQHI_plus(pm25_hourly, detailed = TRUE))
  expect_snapshot(AQHI_plus(pm25_hourly, detailed = TRUE, language = "fr"))
  # Real data with units-class concentrations
  expect_snapshot(example_obs$pm25_1hr |> AQHI_plus(detailed = TRUE))

  expect_equal(AQHI_plus(pm25_hourly, detailed = FALSE), expected_aqhi)
})

test_that("factor contract: full ordered levels, '+' never '11'", {
  expect_equal(levels(AQHI_plus(50, detailed = FALSE)), c(1:10, "+"))

  above_max <- AQHI_plus(c(100.1, 150, 1000), detailed = FALSE)
  expect_equal(above_max, factor(rep("+", 3), levels = c(1:10, "+")))
  expect_false("11" %in% as.character(above_max))

  # Both output paths return the same level factor
  pm25 <- c(10, 25, 35.5, 100, 100.1)
  expect_equal(
    AQHI_plus(pm25, detailed = FALSE),
    AQHI_plus(pm25, detailed = TRUE)$level
  )
})

test_that("invalid and missing input is NA without shifting valid values", {
  # Policy: NA stays NA; NaN, Inf, -Inf and values below min_allowed_pm25
  # (negative by default) are treated as missing.
  invalid <- c(-1, NA, NaN, Inf, -Inf)
  detailed <- AQHI_plus(invalid, detailed = TRUE)
  expect_true(all(is.na(detailed$pm25_1hr_ugm3)))
  expect_true(all(is.na(detailed$level)))
  expect_true(all(is.na(detailed$risk)))
  expect_true(all(is.na(detailed$high_risk_pop_message)))
  expect_equal(detailed$colour, rep("#bbbbbb", length(invalid)))

  # Valid values keep their positions
  mixed <- c(-1, 25, NA, Inf, 50, NaN, 100, -Inf)
  expected_level <- factor(c(NA, "3", NA, NA, "5", NA, "10", NA),
                           levels = c(1:10, "+"))
  expected_risk <- factor(c(NA, "Low", NA, NA, "Moderate", NA, "High", NA),
                          levels = c("Low", "Moderate", "High", "Very High"))
  expect_equal(AQHI_plus(mixed, detailed = FALSE), expected_level)
  expect_equal(AQHI_plus(mixed, detailed = TRUE)$risk, expected_risk)
})

test_that("min_allowed_pm25 sets the lower censoring threshold", {
  pm25 <- c(-20, -10, -5, -0.1, 0, 1, 4.9, 5, 5.1, 100)
  full_levels <- c(1:10, "+")

  expect_equal(
    AQHI_plus(pm25, detailed = FALSE),
    factor(c(NA, NA, NA, NA, 1, 1, 1, 1, 1, 10), levels = full_levels)
  )
  expect_equal(
    AQHI_plus(pm25, min_allowed_pm25 = 5, detailed = FALSE),
    factor(c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 10), levels = full_levels)
  )
})

test_that("detailed output keeps a stable structure", {
  out <- AQHI_plus(c(0, 30, 30.1, 60, 100, 100.1, NA), detailed = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_equal(
    names(out),
    c("pm25_1hr_ugm3", "level", "colour", "risk",
      "high_risk_pop_message", "general_pop_message")
  )
  expect_equal(nrow(out), 7)
  expect_true(is.numeric(out$pm25_1hr_ugm3))
  expect_true(is.factor(out$level))
  expect_true(is.character(out$colour))
  expect_true(is.factor(out$risk))
  expect_true(is.character(out$high_risk_pop_message))
  expect_true(is.character(out$general_pop_message))
})
