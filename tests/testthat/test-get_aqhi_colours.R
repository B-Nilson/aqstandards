# Executable contract for get_aqhi_colours ----------------------------------
# One hex per level 1..10, "+", then grey for missing; PM2.5 values are binned
# through the AQHI+ engine; legacy numeric "11" is treated as "+".
# Tests assert literal expectations only -- never package internals.
#
# Coverage matrix (consolidated when the file was table-ized):
#   - old "expected output returned" (default, levels, NA) -> "each level..."
#   - old "types work" (pm25, mixed, factor inputs)        -> "PM2.5..." + "mixed..."
#   - invalid "3.5"/"x" grey case was probe-only before   -> now in "each level..."

palette <- c(
  "#21C6F5", "#189ACA", "#0D6797", "#FFFD37", "#FFCC2E", "#FE9A3F",
  "#FD6769", "#FF3B3B", "#FF0101", "#CB0713", "#650205", "#bbbbbb"
)
grey <- palette[[12]]

test_that("each level (and missing) maps to its colour", {
  levels_in <- c(1:10, "+")
  expect_equal(get_aqhi_colours(), palette)
  expect_equal(get_aqhi_colours(levels_in), palette[1:11])
  expect_equal(get_aqhi_colours(c(levels_in, NA)), palette)
  # Legacy ordinal "11" spelling of 10+ maps to the "+" colour
  expect_equal(get_aqhi_colours(11), palette[[11]])
  expect_equal(get_aqhi_colours("11"), palette[[11]])
  # Anything without a level is grey
  expect_equal(get_aqhi_colours(c(3.5, "x", NA)), rep(grey, 3))
  # Factor input with missing values
  expect_equal(
    get_aqhi_colours(factor(c(1:10, "+", NA), levels = c(1:10, "+"))),
    palette
  )
})

test_that("PM2.5 concentrations are binned to level colours", {
  pm25 <- c(10, 30, 30.1, 100, 100.1)
  expect_equal(get_aqhi_colours(pm25, types = "pm25_1hr"),
               palette[c(1, 3, 4, 10, 11)])
  # Non-finite and negative PM2.5 are invalid -> grey
  expect_equal(get_aqhi_colours(c(50, Inf, -1), types = "pm25_1hr"),
               c(palette[[5]], grey, grey))
})

test_that("mixed level/PM2.5 input and recycled types work", {
  levels_in <- factor(c(1:10, "+", NA), levels = c(1:10, "+"))
  pm25 <- c(1:11 * 10, NA)
  values <- c(levels_in, pm25)
  types <- c(rep("aqhi", 12), rep("pm25_1hr", 12))

  expect_equal(get_aqhi_colours(values, types = types), rep(palette, 2))
  expect_equal(
    get_aqhi_colours(c("3", "5"), types = "aqhi"),
    palette[c(3, 5)]
  )
})
