#' Calculate the Canadian AQHI+ from hourly \ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}} concentrations
#'
#' @description
#' The Canadian AQHI+ is a modification of the Canadian Air Quality Health Index (\code{\link{AQHI}}).
#' AQHI+ only uses fine particulate matter (\ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}}) instead of the combination of
#' \ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}}, ozone (\ifelse{html}{\out{O<sub>3</sub>}}{\eqn{O_3}}), and nitrogen dioxide (\ifelse{html}{\out{NO<sub>2</sub>}}{\eqn{NO_2}}).
#' Unlike the AQHI which uses 3-hourly mean averages,
#' AQHI+ is calculated using hourly mean averages.
#' The AQHI+ overrides the AQHI if it exceeds the AQHI for a particular hour.
#'
#' The AQHI+ was originally published by Yao et al (2019): \doi{doi:10.17269/s41997-019-00237-w},
#' and has been adopted by all* Canadian provinces/territories as of 2024.
#' (*except Québec where they use the AQI instead of the AQHI/AQHI+).
#'
#' @details
#' This function implements the PM2.5-based AQHI+ mapping described by
#' Yao et al. (2019) and current Environment and Climate Change Canada guidance,
#' and is intended as a stable reference implementation for downstream packages.
#'
#' ## Scientific contract
#'
#' The input is the **1-hour mean \ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}} concentration in
#' \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}}, as a numeric (or units) vector.**
#' AQHI+ levels are `1` through `10`, plus `"+"` for concentrations above 100
#' \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}}.
#' Level bins are 10 \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}} wide with
#' **right-closed boundaries**, so a concentration exactly on a multiple of 10 falls in the bin
#' starting at that value (e.g. 30 is level 3, 30.1 is level 4):
#'
#' | PM2.5 1-hour mean (\ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}}) | AQHI+ level | Risk (en) | Risk (fr) |
#' |---:|:---:|:---|:---|
#' | 0 to 10 | 1 | Low | Faible |
#' | >10 to 20 | 2 | Low | Faible |
#' | >20 to 30 | 3 | Low | Faible |
#' | >30 to 40 | 4 | Moderate | Modéré |
#' | >40 to 50 | 5 | Moderate | Modéré |
#' | >50 to 60 | 6 | Moderate | Modéré |
#' | >60 to 70 | 7 | High | Elevé |
#' | >70 to 80 | 8 | High | Elevé |
#' | >80 to 90 | 9 | High | Elevé |
#' | >90 to 100 | 10 | High | Elevé |
#' | >100 | + | Very High | Très Elevé |
#'
#' The health-relevant concentration boundaries that follow from the binning above are:
#'
#' - **Low**: PM2.5 \ifelse{html}{\out{&le; 30}}{<= 30} (levels 1-3);
#' - **Moderate**: \ifelse{html}{\out{&gt;30}}{>30} to 60 (levels 4-6);
#' - **High**: \ifelse{html}{\out{&gt;60}}{>60} to 100 (levels 7-10);
#' - **Very High**: \ifelse{html}{\out{&gt;100}}{>100} (level `"+"`).
#'
#' Basis (Yao et al. 2019): the level bins are a linear extrapolation anchored on two points,
#' namely that the AQHI+ reaches 4 (moderate risk) once 1-h \ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}}
#' exceeds 30 \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}} (the odour perception threshold for
#' burning softwood; Kistler et al. 2012: \doi{doi:10.1016/j.atmosenv.2012.01.044}) and reaches 7 (high risk)
#' once 1-h \ifelse{html}{\out{PM<sub>2.5</sub>}}{\eqn{PM_{2.5}}} exceeds 60 \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}}.
#'
#' This mapping is the versioned public contract of the package: it must not change
#' without a version bump, regression tests, and release notes.
#'
#' ## Missing and invalid input
#'
#' The invalid-input policy is explicit and deliberate:
#'
#' - **Missing input stays missing.** `NA` values produce `NA` levels/risk and are returned as-is.
#' - **Non-finite values are not valid concentrations.** `NaN`, `Inf`, and `-Inf` are treated as missing (`NA`).
#' - **Concentrations below `min_allowed_pm25` are treated as missing.** The default of 0 therefore treats all
#'   negative concentrations as invalid/missing rather than binning them into level 1.
#'
#' The official/display representation is preserved: `10+` is reported as the level `"+"`, never as a numeric `11`.
#'
#' @inheritParams AQHI
#' @param min_allowed_pm25 (Optional).
#'   A single numeric value indicating the minimum allowed concentration.
#'   All values in `pm25_1hr_ugm3` less than this will be replaced with `NA`.
#'   Default is 0 \ifelse{html}{\out{&mu;g m<sup>-3</sup>}}{\eqn{ug m^{-3}}}, meaning negative values are treated as missing.
#'
#' @references Yao et al (2019): \doi{doi:10.17269/s41997-019-00237-w}
#'
#' Environment and Climate Change Canada: \url{https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html}
#'
#' @return If `detailed = TRUE`:
#' - A tibble (data.frame) with one row per input value and columns:
#'   `pm25_1hr_ugm3` (input concentration after invalid values are set to `NA`),
#'   `level`, `colour`, `risk`, `high_risk_pop_message`, and `general_pop_message`.
#'
#' If `detailed = FALSE`:
#' - A factor vector of AQHI+ levels with `length(pm25_1hr_ugm3)` elements.
#'   Factor levels are ordered `"1"`, `"2"`, ..., `"10"`, `"+"`; missing/invalid
#'   input is returned as `NA` (i.e. `<NA>`).
#'
#' @export
#'
#' @examples
#' # Hourly pm2.5 concentrations
#' pm25 <- example_obs$pm25_1hr[example_obs$site_id == 1]
#' # Calculate the detailed AQHI+ (tibble with risk levels etc)
#' AQHI_plus(pm25)
#' # Or just the AQHI+ (returned as a factor)
#' AQHI_plus(pm25, detailed = FALSE)
AQHI_plus <- function(
  pm25_1hr_ugm3,
  min_allowed_pm25 = 0,
  detailed = TRUE,
  language = "en"
) {
  stopifnot(is.logical(detailed), length(detailed) == 1)
  language <- .check_language(language)

  # Map 1-h PM2.5 to AQHI+ levels. Validation of the concentration input,
  # the invalid-input policy and the binning all live in the engine core
  # aqhi_plus_map() below, which other internals (e.g. get_aqhi_colours)
  # reuse without the renderer. The engine returns the censored concentrations
  # alongside the levels so the detailed output echoes policy-applied input.
  mapped <- aqhi_plus_map(pm25_1hr_ugm3, min_allowed_pm25)

  # Early return if AQHI+ is all thats desired
  if (!detailed) {
    return(mapped$level)
  }

  # Get the associated risk level (low, moderate, high, very high))
  risk <- mapped$level |> get_risk_category(language = language)

  # Combine and return
  dplyr::tibble(
    pm25_1hr_ugm3 = mapped$pm25_1hr_ugm3,
    level = mapped$level,
    colour = mapped$level |> get_aqhi_colours(),
    risk = risk,
    # High risk pop + general pop health warnings
    risk |>
      get_health_messages(language = language) |>
      dplyr::select(-"risk_category")
  )
}

# AQHI+ engine core: apply the invalid-input policy and map 1-h mean PM2.5
# (ug/m3) to AQHI+ levels.
#
# Owns the policy and the binning contract:
#  - NA input stays missing (NA);
#  - non-finite values (NaN, Inf, -Inf) are not valid 1-hour means -> NA;
#  - concentrations below `min_allowed_pm25` (negative values by default) -> NA;
#  - valid concentrations are binned with right-closed cut() intervals every
#    10 ug/m3, so a value exactly on a boundary belongs to the level starting
#    at that value (e.g. 30 is level 3, 100 is level 10, > 100 is "+").
# Returns a list with the policy-applied concentrations and a factor with
# levels .aqhi_levels.
aqhi_plus_map <- function(pm25_1hr_ugm3, min_allowed_pm25 = 0) {
  stopifnot(is.numeric(pm25_1hr_ugm3), length(pm25_1hr_ugm3) > 0)
  stopifnot(is.numeric(min_allowed_pm25), length(min_allowed_pm25) == 1)

  # Missing/invalid input policy
  pm25_1hr_ugm3[is.nan(pm25_1hr_ugm3) | is.infinite(pm25_1hr_ugm3)] <- NA
  pm25_1hr_ugm3[pm25_1hr_ugm3 < min_allowed_pm25] <- NA

  # Level mapping contract (Yao et al. 2019 / ECCC): level k covers PM2.5 in
  # ((k-1)*10, k*10] for k = 2..10, level 1 covers 0-10, and "+" covers > 100.
  aqhi_breakpoints <- c(-Inf, 1:10 * 10, Inf)
  level <- pm25_1hr_ugm3 |>
    cut(
      breaks = aqhi_breakpoints,
      labels = .aqhi_levels,
      right = TRUE
    )

  list(pm25_1hr_ugm3 = pm25_1hr_ugm3, level = level)
}
