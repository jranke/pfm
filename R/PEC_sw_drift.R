#' Calculate predicted environmental concentrations in surface water due to drift
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in surface water based on complete, instantaneous mixing
#' with input via spray drift.
#'
#' @param rate Application rate in units specified below
#' @param applications Number of applications for selection of drift percentile
#' @param drift_percentages Percentage drift values for which to calculate PECsw.
#'   'drift_data' and 'distances' if not NULL.
#' @param drift_data Source of drift percentage data
#' @param crop Crop name (use German names for JKI data), defaults to "Ackerbau"
#' @param distances The distances in m for which to get PEC values
#' @param water_depth Depth of the water body in cm
#' @param rate_units Defaults to g/ha
#' @param PEC_units Requested units for the calculated PEC. Only µg/L currently supported
#' @return The predicted concentration in surface water
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_drift(100)
PEC_sw_drift <- function(rate, 
                         applications = 1,
                         water_depth = 30, 
                         drift_percentages = NULL,
                         drift_data = "JKI",
                         crop = "Ackerbau",
                         distances = c(1, 5, 10, 20),
                         rate_units = "g/ha",
                         PEC_units = "\u00B5g/L")
{
  rate_units <- match.arg(rate_units)
  PEC_units <- match.arg(PEC_units)
  drift_data <- match.arg(drift_data)
  water_volume <- 100 * 100 * (water_depth/100) * 1000   # in L (for 1 ha)
  PEC_sw_overspray <- rate * 1e6 / water_volume          # in µg/L
  dist_index <- as.character(distances)

  if (is.null(drift_percentages)) {
    drift_percentages <- pfm::drift_data_JKI[[applications]][dist_index, crop]
    names(drift_percentages) <- paste(dist_index, "m")
  } else {
    names(drift_percentages) <- paste(drift_percentages, "%")
  }

  PEC_sw_drift <- PEC_sw_overspray * drift_percentages / 100
  return(PEC_sw_drift)
}
