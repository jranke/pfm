#' Calculate predicted environmental concentrations in surface water due to drift
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in surface water based on complete, instantaneous mixing
#' with input via spray drift.
#'
#' It is recommened to specify the arguments `rate`, `water_depth` and
#' `water_width` using [units::units] from the `units` package.
#'
#' @inheritParams drift_percentages_rautmann
#' @importFrom units as_units set_units
#' @seealso [drift_parameters_focus], [drift_percentages_rautmann]
#' @param rate Application rate in units specified below, or with units defined via the
#' `units` package.
#' @param rate_units Defaults to g/ha. For backwards compatibility, only used
#' if the specified rate does not have [units::units]].
#' @param drift_percentages Percentage drift values for which to calculate PECsw.
#'   Overrides 'drift_data' and 'distances' if not NULL.
#' @param drift_data Source of drift percentage data. If 'JKI', the [drift_data_JKI]
#'   included in the package is used. If 'RF', the Rautmann drift data are calculated
#'   either in the original form or integrated over the width of the water body, depending
#'   on the 'formula' argument.
#' @param crop_group_JKI When using the 'JKI' drift data, one of the German names
#'   as used in [drift_data_JKI]. Will only be used if drift_data is 'JKI'.
#' @param water_depth Depth of the water body in cm
#' @param PEC_units Requested units for the calculated PEC. Only µg/L currently supported
#' @param water_width Width of the water body in cm
#' @param side_angle The angle of the side of the water relative to the bottom which
#'   is assumed to be horizontal, in degrees. The SYNOPS model assumes 45 degrees here.
#' @return The predicted concentration in surface water
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
PEC_sw_drift <- function(rate,
  applications = 1,
  water_depth = as_units("30 cm"),
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
  crop_group_JKI = c("Ackerbau",
    "Obstbau frueh", "Obstbau spaet", "Weinbau frueh", "Weinbau spaet",
    "Hopfenbau", "Flaechenkulturen > 900 l/ha", "Gleisanlagen"),
  crop_group_RF = c("arable", "hops", "vines, late", "vines, early",
    "fruit, late", "fruit, early", "aerial"),
  distances = c(1, 5, 10, 20),
  formula = c("Rautmann", "FOCUS"),
  water_width = as_units("100 cm"),
  side_angle = 90,
  rate_units = "g/ha",
  PEC_units = "\u00B5g/L")
{
  rate_units <- match.arg(rate_units)
  PEC_units <- match.arg(PEC_units)
  # Set default units if not specified
  if (!inherits(rate, "units")) rate <- set_units(rate, rate_units, mode = "symbolic")
  if (!inherits(water_width, "units")) water_width <- set_units(water_width, "cm")
  if (!inherits(water_depth, "units")) water_depth <- set_units(water_depth, "cm")
  drift_data <- match.arg(drift_data)
  crop_group_JKI <- match.arg(crop_group_JKI)
  crop_group_RF <- match.arg(crop_group_RF)
  if (drift_data == "JKI" & crop_group_RF != "arable") {
    stop("Specifying crop_group_RF only makes sense if 'RF' is used for 'drift_data'")
  }
  if (drift_data == "RF" & crop_group_JKI != "Ackerbau") {
    stop("Specifying crop_group_JKI only makes sense if 'JKI' is used for 'drift_data'")
  }
  formula <- match.arg(formula)
  if (side_angle < 0 | side_angle > 90) stop("The side anglemust be between 0 and 90 degrees")
  mean_water_width <- if (side_angle == 90) water_width # Mean water width over waterbody depth
  else water_width - (water_depth / tanpi(side_angle/180))
  if (as.numeric(mean_water_width) < 0) stop("Undefined geometry")
  relative_mean_water_width <- mean_water_width / water_width # Always <= 1
  PEC_sw_overspray <- set_units(rate / (relative_mean_water_width * water_depth), PEC_units, mode = "symbolic")
  dist_index <- as.character(distances)

  if (is.null(drift_percentages)) {
    drift_percentages <- switch(drift_data,
      JKI = pfm::drift_data_JKI[[applications]][dist_index, crop_group_JKI],
      RF =  drift_percentages_rautmann(distances, applications,
        formula = formula,
        crop_group_RF, widths = as.numeric(set_units(water_width, "m")))
    )
    names(drift_percentages) <- paste(dist_index, "m")
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
#' @param crop_group_RF One of the crop groups as used in [drift_parameters_focus]
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
  crop_group_RF = c("arable", "hops", "vines, late", "vines, early", "fruit, late",
    "fruit, early", "aerial"),
  formula = c("Rautmann", "FOCUS"),
  widths = 1
)
{
  cg <- match.arg(crop_group_RF)
  if (!applications %in% 1:8) stop("Only 1 to 8 applications are supported")
  formula <- match.arg(formula)

  parms <- pfm::drift_parameters_focus[pfm::drift_parameters_focus$crop_group == cg &
    pfm::drift_parameters_focus$n_apps == applications, c("A", "B", "C", "D", "hinge")]

  if (formula[1] == "Rautmann") {
    drift_percentages = with(as.list(parms), {
      A <- ifelse(distances < hinge, A, C)
      B <- ifelse(distances < hinge, B, D)
      A * distances^B
    })
  } else {
    drift_percentages = with(as.list(parms), {
      z1 = distances
      z2 = distances + widths
      H = hinge
      ifelse(z2 < hinge,
        # farther edge closer than hinge distance
        A/(widths * (B + 1)) * (z2^(B + 1) - z1^(B + 1)),
        ifelse(z1 < hinge,
          # hinge distance in waterbody (between z1 and z2)
          (A/(B + 1) * (H^(B + 1) - z1^(B + 1)) + C/(D + 1) * (z2^(D + 1) - H^(D + 1)))/widths,
          # z1 >= hinge, i.e. near edge farther than hinge distance
          C/(widths * (D + 1)) * (z2^(D + 1) - z1^(D + 1))
        )
      )
    })
  }

  return(drift_percentages)
}
