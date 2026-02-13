utils::globalVariables(c("A", "B", "C", "D", "H", "hinge", "z1", "z2", "distance", "pctg", "width"))
#' Calculate predicted environmental concentrations in surface water due to drift
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in surface water based on complete, instantaneous mixing
#' with input via spray drift.
#'
#' It is recommened to specify the arguments `rate`, `water_depth` and
#' `water_width` using [units::units] from the `units` package.
#'
#' Since pfm version 0.6.5, the function is vectorised with respect to rates,
#' applications, water depth, crop groups and distances
#'
#' @inheritParams drift_percentages_rautmann
#' @importFrom testthat capture_output
#' @importFrom units as_units set_units
#' @seealso [drift_parameters_focus], [drift_percentages_rautmann]
#' @param rate Application rate in units specified below, or with units defined via the
#' `units` package.
#' @param rate_units Defaults to g/ha. For backwards compatibility, only used
#' if the specified rate does not have [units::units]].
#' @param drift_percentages Percentage drift values for which to calculate PECsw.
#'   Overrides 'drift_data', 'distances', 'applications', crop group and
#'   formula arguments if not NULL.
#' @param drift_data Source of drift percentage data. If 'JKI', the [drift_data_JKI]
#'   included in the package is used. If 'RF', the Rautmann drift data are calculated
#'   either in the original form or integrated over the width of the water body, depending
#'   on the 'formula' argument.
#' @param crop_group_JKI When using the 'JKI' drift data, one of the German names
#'   as used in [drift_data_JKI]. Will only be used if drift_data is 'JKI'. Available
#'   crop groups are "Ackerbau", "Obstbau frueh", "Obstbau spaet",
#'   "Weinbau frueh", "Weinbau spaet", "Hopfenbau", "Flaechenkulturen > 900 l/ha" and
#'   "Gleisanlagen".
#' @param water_depth Depth of the water body in cm
#' @param PEC_units Requested units for the calculated PEC. Only µg/L currently supported
#' @param water_width Width of the water body in cm
#' @param side_angle The angle of the side of the water relative to the bottom which
#'   is assumed to be horizontal, in degrees. The SYNOPS model assumes 45 degrees here.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @return A numeric vector with the predicted concentration in surface water.
#'   In some cases, the vector is named with distances or drift percentages, for 
#'   backward compatibility with versions before the vectorisation of arguments
#'   other than 'distances' was introduced in v0.6.5.
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_drift(100)
#' # Alternatively, we can use the formula for a single application to
#' # "Ackerbau" from the paper
#' PEC_sw_drift(100, drift_data = "RF")
#'
#' # This makes it possible to also use different distances
#' PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF")
#'
#' # or consider aerial application
#' PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF",
#'   crop_group_RF = "aerial")
#'
#' # Using custom drift percentages is also supported
#' PEC_sw_drift(100, drift_percentages = c(2.77, 0.95, 0.57, 0.48, 0.29, 0.15, 0.06, 0.03))
#'
#' # The influence of assuming a 45° angle of the sides of the waterbody and the width of the
#' # waterbody can be illustrated
#' PEC_sw_drift(100)
#' PEC_sw_drift(100, drift_data = "RF")
#' PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS")
#' PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45)
#' PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45, water_width = 200)
#'
#' # The function is vectorised with respect to rates, applications, water depth,
#' # crop groups and distances
#' PEC_sw_drift(
#'   rate = rep(100, 6),
#'   applications = c(1, 2, rep(1, 4)),
#'   water_depth = c(30, 30, 30, 60, 30, 30),
#'   crop_group_JKI = c(rep("Ackerbau", 4), rep("Obstbau frueh", 2)),
#'   distances = c(rep(5, 4), 10, 5))
#'
#' # Try the same with the Rautmann formula
#' PEC_sw_drift(
#'   rate = rep(100, 6),
#'   applications = c(1, 2, rep(1, 4)),
#'   water_depth = c(30, 30, 30, 60, 30, 30),
#'   drift_data = "RF",
#'   crop_group_RF = c(rep("arable", 4), rep("fruit, early", 2)),
#'   distances = c(rep(5, 4), 10, 5))
#'
#' # And with the FOCUS variant
#' PEC_sw_drift(
#'   rate = rep(100, 6),
#'   applications = c(1, 2, rep(1, 4)),
#'   water_depth = c(30, 30, 30, 60, 30, 30),
#'   drift_data = "RF",
#'   formula = "FOCUS",
#'   crop_group_RF = c(rep("arable", 4), rep("fruit, early", 2)),
#'   distances = c(rep(5, 4), 10, 5))
PEC_sw_drift <- function(rate,
  applications = 1,
  water_depth = as_units("30 cm"),
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
  crop_group_JKI = "Ackerbau",
  crop_group_RF = "arable",
  distances = c(1, 5, 10, 20),
  formula = c("Rautmann", "FOCUS"),
  water_width = as_units("100 cm"),
  side_angle = 90,
  rate_units = "g/ha",
  PEC_units = "\u00B5g/L")
{

  # Check arguments and set default units if not specified
  rate_units <- match.arg(rate_units)
  PEC_units <- match.arg(PEC_units)
  if (!inherits(rate, "units")) rate <- set_units(rate, rate_units, mode = "symbolic")
  if (!inherits(water_width, "units")) water_width <- set_units(water_width, "cm")
  if (!inherits(water_depth, "units")) water_depth <- set_units(water_depth, "cm")
  drift_data <- match.arg(drift_data)

  unmatched_crop_groups_JKI <- setdiff(crop_group_JKI, colnames(pfm::drift_data_JKI[[1]]))
  if (length(unmatched_crop_groups_JKI) > 0) {
    stop("Crop group(s) ", paste(unmatched_crop_groups_JKI, collapse = ", "), " not supported")
  }

  unmatched_crop_groups_RF <- setdiff(crop_group_RF, unique(pfm::drift_parameters_focus$crop_group))
  if (length(unmatched_crop_groups_RF) > 0) {
    stop("Crop group(s) ", paste(unmatched_crop_groups_RF, collapse = ", "),  "not supported")
  }

  if (drift_data == "JKI" & crop_group_RF[1] != "arable") {
    stop("Specifying crop_group_RF only makes sense if 'RF' is used for 'drift_data'")
  }
  if (drift_data == "RF" & crop_group_JKI[1] != "Ackerbau") {
    stop("Specifying crop_group_JKI only makes sense if 'JKI' is used for 'drift_data'")
  }
  formula <- match.arg(formula)

  # Check waterbody arguments and calculate mean water width (absolute and relative to water width)
  if (side_angle < 0 | side_angle > 90) stop("The side anglemust be between 0 and 90 degrees")
  mean_water_width <- if (side_angle == 90) water_width # Mean water width over waterbody depth
  else water_width - (water_depth / tanpi(side_angle/180))
  if (as.numeric(mean_water_width) < 0) stop("Undefined geometry")
  relative_mean_water_width <- mean_water_width / water_width # Always <= 1
  
  # Check lengths of arguments advertised as vectorised for compatibility
  arg_lengths <- sapply(
    list(rate = rate, applications = applications, distances = distances, 
      water_depth = water_depth, crop_group_JKI = crop_group_JKI, 
      crop_group_RF = crop_group_RF),
    length)
  
  arg_lengths_not_one <- arg_lengths[arg_lengths != 1]
  if (length(unique(arg_lengths_not_one)) > 1) {
    stop("The following argument lengths do not match:\n", 
      capture_output(print(arg_lengths_not_one)))
  }

  # Base PEC sw drift for overspray
  PEC_sw_overspray <- set_units(rate / (relative_mean_water_width * water_depth), PEC_units, mode = "symbolic")

  if (is.null(drift_percentages)) {
    if (drift_data == "JKI") {
      drift_data_JKI_long <- pfm::drift_data_JKI |>
        lapply(as_tibble, rownames = "distance") |>
        bind_rows(.id = "applications") |>
        pivot_longer(3:10, names_to = "crop_group_JKI", values_to = "pctg")

      drift_percentages <- tibble(
          applications = as.character(applications),
          distance = as.character(distances), crop_group_JKI
        ) |>
        left_join(drift_data_JKI_long, by = c("applications", "distance", "crop_group_JKI")) |>
        pull(pctg)
      names(drift_percentages) <- paste(distances, "m")
    }
    if (drift_data == "RF") {
      drift_percentages <- drift_percentages_rautmann(distances, applications,
        formula = formula,
        crop_group_RF, widths = as.numeric(set_units(water_width, "m")))
      names(drift_percentages) <- paste(distances, "m")
    }
  } else {
    names(drift_percentages) <- paste(drift_percentages, "%")
  }

  PEC_sw_drift <- PEC_sw_overspray * drift_percentages / 100
  return(PEC_sw_drift)
}

#' Calculate drift percentages based on Rautmann data
#'
#' @param formula By default, the original Rautmann formula is used. If you
#' specify "FOCUS", mean drift input over the width of the water body is
#' calculated as described in Chapter 5.4.5 of the FOCUS surface water guidance
#' @param distances The distances in m for which to get PEC values
#' @param widths The widths of the water bodies (only used in the FOCUS formula)
#' @param applications Number of applications for selection of drift percentile
#' @param crop_group_RF Crop group(s) as used in [drift_parameters_focus], i.e.
#'   "arable", "hops", "vines, late", "vines, early", "fruit, late", "fruit, early"
#'   or "aerial".
#' @importFrom tibble tibble
#' @importFrom dplyr if_else left_join mutate pull
#' @seealso [drift_parameters_focus], [PEC_sw_drift]
#' @references FOCUS (2014) Generic guidance for Surface Water Scenarios (version 1.4).
#' FOrum for the Co-ordination of pesticde fate models and their USe.
#' <http://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/Generic%20FOCUS_SWS_vc1.4.pdf>
#' @export
#' @examples
#' # Compare JKI data with Rautmann and FOCUS formulas for arable crops (default)
#' # One application on field crops, for 1 m, 3 m and 5 m distance
#' drift_data_JKI[[1]][as.character(c(1, 3, 5)), "Ackerbau"]
#' drift_percentages_rautmann(c(1, 3, 5))
#' drift_percentages_rautmann(c(1, 3, 5), formula = "FOCUS")
#'
#' # Since pfm 0.6.5, the function can also take a vector of crop groups
#' drift_percentages_rautmann(
#'   distances = c(1, 5, 5),
#'   crop_group_RF = c("fruit, early", "fruit, early", "fruit, late"))
#'
#' # Two applications, all else equal
#' drift_data_JKI[[2]][as.character(c(1, 3, 5)), "Ackerbau"]
#' drift_percentages_rautmann(c(1, 3, 5), applications = 2)
#' drift_percentages_rautmann(c(1, 3, 5), formula = "FOCUS", app = 2)
#'
#' # One application to early or late fruit crops
#' drift_data_JKI[[1]][as.character(c(3, 5, 20, 50)), "Obstbau frueh"]
#' drift_percentages_rautmann(c(3, 5, 20, 50), crop_group_RF = "fruit, early")
#' drift_percentages_rautmann(c(3, 5, 20, 50), crop_group_RF = "fruit, early",
#'   formula = "FOCUS")
#' drift_data_JKI[[1]][as.character(c(3, 5, 20, 50)), "Obstbau spaet"]
#' drift_percentages_rautmann(c(3, 5, 20, 50), crop_group_RF = "fruit, late")
#' drift_percentages_rautmann(c(3, 5, 20, 50), crop_group_RF = "fruit, late",
#'   formula = "FOCUS")
#'
#' # We get a continuum if the waterbody covers the hinge distance
#' # (11.4 m for 1 early app to fruit)
#' x <- seq(3, 30, by = 0.1)
#' d <- drift_percentages_rautmann(x, crop_group_RF = "fruit, early", formula = "FOCUS")
#' plot(x, d, type = "l",
#'   xlab = "Distance of near edge [m]",
#'   ylab = "Mean drift percentage over waterbody width",
#'   main = "One application to fruit, early")
#' abline(v = 11.4, lty = 2)
drift_percentages_rautmann <- function(distances, applications = 1,
  crop_group_RF = "arable",
  formula = c("Rautmann", "FOCUS"),
  widths = 1
)
{
  unmatched_crop_groups <- setdiff(crop_group_RF, unique(pfm::drift_parameters_focus$crop_group))
  if (length(unmatched_crop_groups) > 0) stop("Crop group(s) ", unmatched_crop_groups, " not supported")
  if (!all(applications %in% 1:8)) stop("Only 1 to 8 applications are supported")
  formula <- match.arg(formula)

  # To avoid recycling of components with length != 1 but smaller than the longest argument,
  # which would likely be unintended, we use tibble here
  parms <- tibble(distance = distances, width = widths, n_apps = applications, crop_group = crop_group_RF) |>
    left_join(pfm::drift_parameters_focus, by = c("n_apps", "crop_group"))

  if (formula[1] == "Rautmann") {
    drift_percentages <- parms |>
      mutate(
        A = if_else(distance < hinge, A, C),
        B = if_else(distance < hinge, B, D)) |>
      mutate(
        pctg = A * distances^B) |>
      pull(pctg)
  } else {
    drift_percentages <- parms |>
      mutate(
        z1 = distance,
        z2 = distance + width,
        H = hinge) |>
      mutate(
        pctg = if_else(z2 < hinge,
        # farther edge closer than hinge distance
        A/(width * (B + 1)) * (z2^(B + 1) - z1^(B + 1)),
        if_else(z1 < hinge,
          # hinge distance in waterbody (between z1 and z2)
          (A/(B + 1) * (H^(B + 1) - z1^(B + 1)) + C/(D + 1) * (z2^(D + 1) - H^(D + 1)))/width,
          # z1 >= hinge, i.e. near edge farther than hinge distance
          C/(width * (D + 1)) * (z2^(D + 1) - z1^(D + 1)))
        )) |>
      pull(pctg)
  }

  return(drift_percentages)
}
