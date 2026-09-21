# US AQI ------------------------------------------------------------------

# TODO: Include AQI health messaging
# TODO: add @description, @family
# TODO: Add reference to https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf
# Example:
# AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)

#' Calculate the US AQI from pollutant observations
#'
#' @param dates Vector of hourly dates corresponding to observations. Date gaps will be filled automatically.
#' @param o3_8hr_ppm (Optional) Numeric vector of hourly 8-hour mean ozone (O3) concentrations (ppm).
#' Will be calculated from o3_1hr_ppm if provided and o3_8hr_ppm not provided.
#' @param o3_1hr_ppm (Optional) Numeric vector of hourly 1-hour mean ozone (O3) concentrations (ppm).
#' @param pm25_24hr_ugm3 (Optional) Numeric vector of hourly 24-hour mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' Will be calculated from pm25_1hr_ugm3 if provided and pm25_24hr_ugm3 not provided.
#' @param pm25_1hr_ugm3 (Optional) Numeric vector of hourly 1-hour mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param pm10_24hr_ugm3 (Optional) Numeric vector of hourly 24-hour mean coarse particulate matter (PM10) concentrations (ug/m^3).
#' Will be calculated from pm10_1hr_ugm3 if provided and pm10_24hr_ugm3 not provided.
#' @param pm10_1hr_ugm3 (Optional) Numeric vector of hourly 1-hour mean coarse particulate matter (PM10) concentrations (ug/m^3).
#' @param co_8hr_ppm (Optional) Numeric vector of hourly 8-hour mean carbon monoxide (CO) concentrations (ppm).
#' Will be calculated from co_1hr_ppm if provided and co_8hr_ppm not provided.
#' @param co_1hr_ppm (Optional) Numeric vector of hourly 1-hour mean carbon monoxide (CO) concentrations (ppm).
#' @param so2_1hr_ppb (Optional) Numeric vector of hourly 1-hour mean sulfur dioxide (SO2) concentrations (ppb).
#' 24-hour averages will be calculated automatically.
#' @param no2_1hr_ppb (Optional) Numeric vector of hourly 1-hour mean nitrogen dioxide (NO2) concentrations (ppb).
#'
#' @description
#' The Air Quality Index (AQI) is used for reporting on air quality in the United States,
#' and focuses on short term health effects as a result of breathing polluted air.
#'
#' The AQI is calculated separately for 5 pollutants: ozone (O3), particulate matter (PM2.5 and PM10),
#' carbon monoxide (CO), sulfur dioxide (SO2), and nitrogen dioxide (NO2) based
#' on maximum values (of various averaging periods) for a particular day.
#' The highest AQI value among each pollutants value for that day is recorded
#' as the AQI and the corresponding pollutant is reported as the principal pollutant.
#'
#' The US EPA has established risk categories and associated health messaging for AQI ranges including:
#' "Good" (0-50), "Moderate" (51-100), "Unhealthy for Sensitive Groups" (101-150),
#' "Unhealthy" (151-200), "Very Unhealthy" (200-300), and "Hazardous" (301-500).
#' AQI values above 500 are considered "Beyond the AQI",
#' but AQI values will still be calculated for relative comparisons.
#'
#' @return A tibble (data.frame) with columns:
#' date, AQI, risk_category, principal_pol
#' and 1 row for each day between the min and max values of the provided dates
#' @export
#'
#' @family Air Quality Standards
#' @family USA Air Quality
#'
#' @examples
#' AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
#' AQI(o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
AQI <- function(
  dates = Sys.time(),
  o3_8hr_ppm = NA,
  o3_1hr_ppm = NA,
  pm25_24hr_ugm3 = NA,
  pm25_1hr_ugm3 = NA,
  pm10_24hr_ugm3 = NA,
  pm10_1hr_ugm3 = NA,
  co_8hr_ppm = NA,
  co_1hr_ppm = NA,
  so2_1hr_ppb = NA,
  no2_1hr_ppb = NA
) {
  # TODO: Reference https://forum.airnowtech.org/t/the-aqi-equation-2024-valid-beginning-may-6th-2024/453

  # Determine which pollutants provided as input
  AQI_pols <- methods::formalArgs(AQI)[-1]
  all_missing <- AQI_pols |>
    handyr::for_each(
      .as_list = TRUE,
      .name = TRUE,
      .show_progress = FALSE,
      \(pol) all(is.na(get(pol)))
    )
  all_missing$so2_24hr_ppb <- TRUE
  # Ensure at least one pollutants data is provided
  if (all(unlist(all_missing))) {
    stop(paste(
      "At least one pollutant's concentrations must",
      "be provided and have at least 1 non-NA value."
    ))
  }

  # Combine inputs and fill date gaps with NAs
  dat <- dplyr::tibble(
    date = dates,
    o3_8hr_ppm,
    o3_1hr_ppm,
    pm25_24hr_ugm3,
    pm25_1hr_ugm3,
    pm10_24hr_ugm3,
    pm10_1hr_ugm3,
    co_8hr_ppm,
    co_1hr_ppm,
    so2_1hr_ppb,
    so2_24hr_ppb = NA,
    no2_1hr_ppb
  ) |>
    tidyr::complete(date = seq(min(dates), max(dates), "1 hours")) |>
    dplyr::arrange(.data$date)

  # Make additional running averages if needed
  needs_o3_8hr <- all_missing$o3_8hr_ppm &
    !all_missing$o3_1hr_ppm &
    sum(!is.na(dat$o3_1hr_ppm)) >= 5
  if (needs_o3_8hr) {
    dat$o3_8hr_ppm <- dat$o3_1hr_ppm |>
      handyr::rolling("mean", .width = 8, .min_non_na = 5)
    all_missing$o3_8hr_ppm <- FALSE
  }
  needs_pm25_24hr <- all_missing$pm25_24hr_ugm3 &
    !all_missing$pm25_1hr_ugm3 &
    sum(!is.na(dat$pm25_1hr_ugm3)) >= 15
  if (needs_pm25_24hr) {
    dat$pm25_24hr_ugm3 <- dat$pm25_1hr_ugm3 |>
      handyr::rolling("mean", .width = 24, .min_non_na = 15)
    all_missing$pm25_24hr_ugm3 <- FALSE
  }
  needs_pm10_24hr <- all_missing$pm10_24hr_ugm3 &
    !all_missing$pm10_1hr_ugm3 &
    sum(!is.na(dat$pm10_1hr_ugm3)) >= 15
  if (needs_pm10_24hr) {
    dat$pm10_24hr_ugm3 <- dat$pm10_1hr_ugm3 |>
      handyr::rolling("mean", .width = 24, .min_non_na = 15)
    all_missing$pm10_24hr_ugm3 <- FALSE
  }
  needs_co_8hr <- all_missing$co_8hr_ppm &
    !all_missing$co_1hr_ppm &
    sum(!is.na(dat$co_1hr_ppm)) >= 5
  if (needs_co_8hr) {
    dat$co_8hr_ppm <- dat$co_1hr_ppm |>
      handyr::rolling("mean", .width = 8, .min_non_na = 5)
    all_missing$co_8hr_ppm <- FALSE
  }
  needs_so2_24hr <- !all_missing$so2_1hr_ppb &
    sum(!is.na(dat$so2_1hr_ppb)) >= 15
  if (needs_so2_24hr) {
    dat$so2_24hr_ppb <- dat$so2_1hr_ppb |>
      handyr::rolling("mean", .width = 24, .min_non_na = 15)
    all_missing$so2_24hr_ppb <- FALSE
  }

  # Daily aggregation (EPA TAD: the AQI is based on "daily air quality
  # summaries, specifically daily maximums or daily averages"). The 1-hour
  # SO2 daily max is captured here for the fixed-at-200 exception below;
  # the remaining daily-max metrics are converted in the follow-up commit.
  dat <- dat |>
    dplyr::group_by(
      date = .data$date |>
        lubridate::floor_date("days")
    ) |>
    # Per-day max of the 1-hour SO2 series, computed on the raw hourly
    # values BEFORE summarise -- inside summarise, later expressions see
    # already-summarised columns, so the max there would be the max of the
    # daily mean. This feeds the fixed-at-200 exception below.
    dplyr::mutate(
      so2_1hr_max_ppb = suppressWarnings(max(so2_1hr_ppb, na.rm = TRUE))
    ) |>
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::everything(),
        \(x) mean(x, na.rm = TRUE)
      )
    ) |>
    # Truncate daily means
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("o3"),
        \(x) handyr::truncate(x, digits = 3)
      ),
      dplyr::across(
        dplyr::starts_with("pm25|co"),
        \(x) handyr::truncate(x, digits = 1)
      ),
      dplyr::across(
        dplyr::starts_with("so2|no2|pm10"),
        \(x) handyr::truncate(x, digits = 0)
      )
    )

  # Calculate AQI for each pollutant provided (so2_1hr_max_ppb is the
  # daily-max helper for the SO2 exception, not a classifiable pollutant)
  pols <- setdiff(names(dat), c("date", "so2_1hr_max_ppb"))
  for (pol in pols) {
    if (!all_missing[[pol]]) {
      dat <- AQI_from_con(dat, pol)
    } else {
      dat[[paste0("AQI_", pol)]] <- NA
    }
  }

  AQI_cols <- paste0("AQI_", pols)
  names(AQI_cols) <- (AQI_cols |>
    stringr::str_split("_", simplify = TRUE))[, 2]

  # SO2 exception (TAD, "How do I calculate AQI values for SO2?"): on a day
  # where the daily max 1-hour concentration is at or above 305 ppb but the
  # 24-hour average is not, use 200 for both AQI breakpoints -- "This
  # effectively fixes the AQI value at 200 exactly, which ensures that you
  # get the highest possible AQI value associated with your 1-hour
  # concentration on such days."
  if (!all_missing$so2_1hr_ppb) {
    exception <- !is.na(dat$so2_1hr_max_ppb) &
      dat$so2_1hr_max_ppb >= 305 &
      (is.na(dat$so2_24hr_ppb) | dat$so2_24hr_ppb < 305)
    dat$AQI_so2_1hr_ppb[exception] <- 200
    dat$cat_so2_1hr_ppb[exception] <- "Unhealthy"
  }

  # Set hourly AQI to the highest of the calculated values
  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # An all-NA row (every sub-index missing) is a documented NA result,
      # not a warning-worthy event; max() warns, so silence it here.
      AQI = suppressWarnings(max(
        dplyr::c_across(dplyr::all_of(unname(AQI_cols))),
        na.rm = TRUE
      )) |>
        handyr::swap(Inf, with = NA) |>
        handyr::swap(-Inf, with = NA),
      risk_category = AQI_risk_category(.data$AQI)
    ) |>
    get_AQI_principal_pol(AQI_cols) |>
    dplyr::ungroup() |>
    dplyr::select("date", "AQI", "risk_category", "principal_pol")
}

## AQI Helpers ------------------------------------------------------------
# EPA TAD 2018 Table 4 / 2024 equation post (AQI category index ranges).
# "Beyond the AQI" is defined for AQI *higher than* 500 (TAD FAQ), so it
# starts at 501 -- the previous 301:500 / 500:5000 ranges duplicated 500.
aqi_levels <- list(
  "Good" = 0:50,
  "Moderate" = 51:100,
  "Unhealthy for Sensitive Groups" = 101:150,
  "Unhealthy" = 151:200,
  "Very Unhealthy" = 201:300,
  "Hazardous" = 301:500,
  "Beyond the AQI" = 501:5000
)

# Returns Risk category when AQI value provided
AQI_risk_category <- function(AQI) {
  labels <- seq_along(aqi_levels) |>
    sapply(\(i) {
      names(aqi_levels)[i] |>
        stringr::str_remove("2$") |>
        rep(length(aqi_levels[[i]]))
    })
  factor(AQI, levels = unlist(aqi_levels), labels = unlist(labels))
}

# Get risk category for breakpoint determination for AQI formulation
AQI_bp_cat <- function(obs, bps) {
  handyr::silence(
    output = FALSE,
    bps$risk_category |>
      lapply(\(cat) {
        bp <- bps[bps$risk_category == cat, ]
        ifelse(obs >= bp$bp_low & obs <= bp$bp_high, cat, NA)
      }) |>
      as.data.frame() |>
      apply(1, \(row) {
        ifelse(all(is.na(row)), NA, row[!is.na(row)])
      }) |>
      unlist()
  )
}

# When provided concentrations and corresponding breakpoints, return AQI.
# EPA TAD 2018 (step d): "Round the index to the nearest integer"; the 2024
# equation post likewise: "The resulting AQI is rounded to the nearest whole
# number". (The TAD's own worked example prints 148 for 147.487, but the
# normative text of both eras says round.) Ties at .5: the sources are
# silent, so conventional half-up rounding is the documented interpretation
# choice (R's round() would tie-break to even).
AQI_formulation <- function(obs, bp_low, bp_high, aqi_low, aqi_high) {
  floor(
    (aqi_high - aqi_low) / (bp_high - bp_low) * (obs - bp_low) + aqi_low + 0.5
  )
}

# Workhorse function to classify concentrations into breakpoint rows,
# append the corresponding breakpoints, and calculate the AQI sub-index
AQI_from_con <- function(dat, pol) {
  bps <- AQI_breakpoints[[pol]]
  if (is.null(bps)) {
    # No breakpoint table: this column is an intermediate input only (e.g.
    # pm25_1hr_ugm3 feeding the 24-hour mean), so it has no sub-index.
    dat[[paste0("cat_", pol)]] <- NA_character_
    dat[[paste0("AQI_", pol)]] <- NA_real_
    return(dat)
  }
  # "Beyond the AQI" rows (bp_high = Inf): per the TAD FAQ, concentrations
  # above the Hazardous range "use the same linear relationship that is
  # used for the Hazardous category" -- so extend the final closed
  # segment's slope through the open row.
  is_open <- is.infinite(bps$bp_high)
  if (any(is_open)) {
    last_closed <- max(which(!is_open))
    bps$bp_high[is_open] <-
      bps$bp_low[is_open] + (bps$bp_high[last_closed] - bps$bp_low[last_closed])
    bps$aqi_high[is_open] <-
      bps$aqi_low[is_open] + (bps$aqi_high[last_closed] - bps$aqi_low[last_closed])
  }
  # Classify each concentration into one breakpoint row. Missing values
  # stay missing (an NA concentration is an NA sub-index, never 0), and
  # values in a TAD table's "blank place" (a gap between rows, e.g. 8-hour
  # ozone above its 0.200 ppm cap) match no row and stay NA.
  bp_row <- apply(
    outer(dat[[pol]], bps$bp_low, `>=`) & outer(dat[[pol]], bps$bp_high, `<=`),
    1,
    \(hits) {
      if (any(is.na(hits))) NA_integer_
      else if (any(hits)) which(hits)
      else NA_integer_
    }
  )
  dat[[paste0("bp_row_", pol)]] <- bp_row
  dat <- dat |>
    dplyr::left_join(
      bps |>
        dplyr::mutate(.bp_row = dplyr::row_number()) |>
        dplyr::rename_with(.cols = 2:5, \(x) paste0(x, "_", pol)),
      by = stats::setNames(".bp_row", paste0("bp_row_", pol))
    )
  # Rename the joined row label for provenance and calculate the sub-index
  dat <- dat |> dplyr::rename(!!paste0("cat_", pol) := risk_category)
  dat[[paste0("AQI_", pol)]] <- AQI_formulation(
    obs = dat[[pol]],
    bp_low = dat[[paste0("bp_low_", pol)]],
    bp_high = dat[[paste0("bp_high_", pol)]],
    aqi_low = dat[[paste0("aqi_low_", pol)]],
    aqi_high = dat[[paste0("aqi_high_", pol)]]
  )
  dat
}

# Classify concentrations and compute each pollutant sub-index. The
# breakpoint tables quote the EPA TAD 2018 Table 5 (Table 4 of the
# document text) with the 2024 PM2.5 revision (effective May 6, 2024;
# package implements the current AQI only -- see NEWS). Each TAD row is
# one table row; the TAD's two Hazardous rows (301-400, 401-500) are kept
# separate so interpolation follows the sourced segments. An open-ended
# final row (bp_high = Inf, aqi_low = 501) marks a table that extends
# "Beyond the AQI": per the TAD FAQ, values above the final closed row
# continue the final segment's linear relationship. Tables without one
# are capped where the TAD ends them: 8-hour O3 defines nothing >= 301
# (TAD Table 5 footnote 2), 1-hour SO2 nothing >= 201, and 24-hour SO2
# nothing above 1004 ppb (TAD "How do I calculate AQI values for SO2?").
AQI_breakpoints <- list(
  ## 8 hour mean ozone
  o3_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[1:5],
    bp_low = c(0, 0.055, 0.071, 0.086, 0.106),
    bp_high = c(0.054, 0.07, 0.085, 0.105, 0.2),
    aqi_low = c(0, 51, 101, 151, 201),
    aqi_high = c(50, 100, 150, 200, 300)
  ),
  ## 1 Hour Mean Ozone
  # 1-hour O3 has no Good/Moderate rows: concentrations below 0.125 ppm
  # are disregarded (TAD "What do I do with concentrations ... blank
  # places in the table?").
  o3_1hr_ppm = data.frame(
    risk_category = names(aqi_levels)[c(3:5, 6, 6, 7)],
    bp_low = c(0.125, 0.165, 0.205, 0.405, 0.505, 0.605),
    bp_high = c(0.164, 0.204, 0.404, 0.504, 0.604, Inf),
    aqi_low = c(101, 151, 201, 301, 401, 501),
    aqi_high = c(150, 200, 300, 400, 500, Inf)
  ),
  ## 24 Hour Mean Fine Particulate Matter (2024 revision, effective May 6, 2024)
  pm25_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[c(1:5, 6, 6, 7)],
    bp_low = c(0, 9.1, 35.5, 55.5, 125.5, 225.5, 325.5, 500.5),
    bp_high = c(9, 35.4, 55.4, 125.4, 225.4, 325.4, 500.4, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 401, 501),
    aqi_high = c(50, 100, 150, 200, 300, 400, 500, Inf)
  ),
  ## 24 Hour Mean Coarse Particulate Matter
  pm10_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[c(1:5, 6, 6, 7)],
    bp_low = c(0, 55, 155, 255, 355, 425, 505, 605),
    bp_high = c(54, 154, 254, 354, 424, 504, 604, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 401, 501),
    aqi_high = c(50, 100, 150, 200, 300, 400, 500, Inf)
  ),
  ## 8 Hour Mean Carbon Monoxide
  co_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[c(1:5, 6, 6, 7)],
    bp_low = c(0, 4.5, 9.5, 12.5, 15.5, 30.5, 40.5, 50.5),
    bp_high = c(4.4, 9.4, 12.4, 15.4, 30.4, 40.4, 50.4, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 401, 501),
    aqi_high = c(50, 100, 150, 200, 300, 400, 500, Inf)
  ),
  ## 1 Hour Mean Sulfur Dioxide
  # 1-hour SO2 defines nothing >= 201: the upper end of the SO2 AQI uses
  # 24-hour average concentrations (TAD "How do I calculate AQI values
  # for SO2?"). A daily max 1-hour concentration at or above 305 ppb
  # whose 24-hour average stays below 305 ppb is handled by the wrapper
  # (fixed at AQI 200 per the TAD).
  so2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[1:4],
    bp_low = c(0, 36, 76, 186),
    bp_high = c(35, 75, 185, 304),
    aqi_low = c(0, 51, 101, 151),
    aqi_high = c(50, 100, 150, 200)
  ),
  ## 24 Hour Mean Sulfur Dioxide
  # AQI >= 201 is calculated with 24-hour SO2 concentrations.
  so2_24hr_ppb = data.frame(
    risk_category = names(aqi_levels)[c(5, 6, 6, 7)],
    bp_low = c(305, 605, 805, 1005),
    bp_high = c(604, 804, 1004, Inf),
    aqi_low = c(201, 301, 401, 501),
    aqi_high = c(300, 400, 500, Inf)
  ),
  ## 1 Hour Mean Nitrogen Dioxide
  no2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[c(1:5, 6, 6, 7)],
    bp_low = c(0, 54, 101, 361, 650, 1250, 1650, 2050),
    bp_high = c(53, 100, 360, 649, 1249, 1649, 2049, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 401, 501),
    aqi_high = c(50, 100, 150, 200, 300, 400, 500, Inf)
  )
)

get_AQI_principal_pol <- function(dat, AQI_cols) {
  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      principal_pol_index = which.max(
        dplyr::across(
          dplyr::all_of(unname(AQI_cols)),
          \(x) x |> handyr::swap(NA, with = 0)
        )
      ),
      principal_pol = names(AQI_cols)[.data$principal_pol_index],
      principal_pol = (!is.na(.data$AQI)) |>
        ifelse(.data$principal_pol, NA) |>
        factor(unique(names(AQI_cols)))
    )
}
