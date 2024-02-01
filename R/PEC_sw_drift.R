#' Calculate predicted environmental concentrations in surface water due to drift
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in surface water based on complete, instantaneous mixing
#' with input via spray drift.
#'
#' @inheritParams drift_percentages_rautmann_formula
#' @seealso [drift_parameters_focus], [drift_percentages_rautmann_formula]
#' @param rate Application rate in units specified below
#' @param drift_percentages Percentage drift values for which to calculate PECsw.
#'   'drift_data' and 'distances' if not NULL.
#' @param drift_data Source of drift percentage data. If 'JKI', the [drift_data_JKI]
#'   included in the package is used. If 'RF', the Rautmann formula is used, if
#'   implemented for the crop type and number of applications
#' @param crop_group_JKI When using the 'JKI' drift data, one of the German names
#'   as used in [drift_parameters_focus].
#' @param water_depth Depth of the water body in cm
#' @param rate_units Defaults to g/ha
#' @param PEC_units Requested units for the calculated PEC. Only µg/L currently supported
#' @return The predicted concentration in surface water
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_drift(100)
#' # Alternatively, we can use the formula for a single application to "Ackerbau" from the paper
#' PEC_sw_drift(100, drift_data = "RF")
#'
#' # This makes it possible to also use different distances
#' PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF")
#'
#' # or consider aerial application
#' PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF", 
#'   crop_group_focus = "aerial")
#'
#' # Using custom drift percentages is also supported
#' PEC_sw_drift(100, drift_percentages = c(2.77, 0.95, 0.57, 0.48, 0.29, 0.15, 0.06, 0.03))
PEC_sw_drift <- function(rate,
  applications = 1,
  water_depth = 30,
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
  crop_group_JKI = c("Ackerbau", 
    "Obstbau frueh", "Obstbau spaet", "Weinbau frueh", "Weinbau spaet",
    "Hopfenbau", "Flaechenkulturen > 900 l/ha", "Gleisanlagen"),
  crop_group_focus = c("arable", "hops", "vines, late", "vines, early", 
    "fruit, late", "fruit, early", "aerial"),
  distances = c(1, 5, 10, 20),
  rate_units = "g/ha",
  PEC_units = "\u00B5g/L")
{
  rate_units <- match.arg(rate_units)
  PEC_units <- match.arg(PEC_units)
  drift_data <- match.arg(drift_data)
  crop_group_JKI <- match.arg(crop_group_JKI)
  crop_group_focus <- match.arg(crop_group_focus)
  water_volume <- 100 * 100 * (water_depth/100) * 1000   # in L (for 1 ha)
  PEC_sw_overspray <- rate * 1e6 / water_volume          # in µg/L
  dist_index <- as.character(distances)

  if (is.null(drift_percentages)) {
    drift_percentages <- switch(drift_data,
      JKI = pfm::drift_data_JKI[[applications]][dist_index, crop_group_JKI],
      RF =  drift_percentages_rautmann_formula(distances, applications, crop_group_focus)
    )
    names(drift_percentages) <- paste(dist_index, "m")
  } else {
    names(drift_percentages) <- paste(drift_percentages, "%")
  }

  PEC_sw_drift <- PEC_sw_overspray * drift_percentages / 100
  return(PEC_sw_drift)
}

#' Calculate the drift percentages according to the Rautmann formula
#'
#' @param distances The distances in m for which to get PEC values
#' @param applications Number of applications for selection of drift percentile
#' @param crop_group_focus One of the crop groups as used in [drift_parameters_focus]
#' @seealso [drift_parameters_focus], [PEC_sw_drift]
#' @export
#' @examples
#' # Compare JKI data with Rautmann formula
#' # One application on field crops, for 1 m, 3 m and 5 m distance
#' drift_data_JKI[[1]][as.character(c(1, 3, 5)), "Ackerbau"]
#' drift_percentages_rautmann_formula(c(1, 3, 5))
#'
#' # One application to early or late fruit crops
#' drift_data_JKI[[1]][as.character(c(3, 5, 20, 50)), "Obstbau frueh"]
#' drift_percentages_rautmann_formula(c(3, 5, 20, 50), crop_group = "fruit, early")
#' drift_data_JKI[[1]][as.character(c(3, 5, 20, 50)), "Obstbau spaet"]
#' drift_percentages_rautmann_formula(c(3, 5, 20, 50), crop_group = "fruit, late")
drift_percentages_rautmann_formula <- function(distances, applications = 1, 
  crop_group_focus = c("arable", "hops", "vines, late", "vines, early", "fruit, late",
    "fruit, early", "aerial"))
{
  cg <- match.arg(crop_group_focus)
  if (!applications %in% 1:8) stop("Only 1 to 8 applications are supported")
  parms <- pfm::drift_parameters_focus[pfm::drift_parameters_focus$crop_group == cg &
    pfm::drift_parameters_focus$n_apps == applications, c("A", "B", "C", "D", "hinge")]

  drift_percentages = with(as.list(parms), {
      A <- ifelse(distances < hinge, A, C)
      B <- ifelse(distances < hinge, B, D)
      A * distances^B
    }
  )
  return(drift_percentages)
}
