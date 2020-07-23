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
#' @param drift_data Source of drift percentage data. If 'JKI', the [drift_data_JKI]
#'   included in the package is used. If 'RF', the Rautmann formula is used, if
#'   implemented for the crop type and number of applications
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
#' # Alternatively, we can use the formula for a single application to "Ackerbau" from the paper
#' PEC_sw_drift(100, drift_data = "RF")
#'
#' # This makes it possible to also use different substances
#' PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF")
#'
#' # Using custom drift percentages is also supported
#' PEC_sw_drift(100, drift_percentages = c(2.77, 0.95, 0.57, 0.48, 0.29, 0.15, 0.06, 0.03))
PEC_sw_drift <- function(rate,
  applications = 1,
  water_depth = 30,
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
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

  RF <- list(
    "1" = list("Ackerbau" = function(distance) 2.7705 * distance^-0.9787) # p. 134
  )
  if (drift_data == "RF") {
    if (is.null(RF[[as.character(applications)]])) stop("Rautmann formula not included for ", applications, " applications")
    if (is.null(RF[[as.character(applications)]][[crop]])) stop("Rautmann formula not included for this case")
  }

  if (is.null(drift_percentages)) {
    drift_percentages <- switch(drift_data,
      JKI = pfm::drift_data_JKI[[applications]][dist_index, crop],
      RF =  RF[[applications]][[crop]](distances)
    )
    names(drift_percentages) <- paste(dist_index, "m")
  } else {
    names(drift_percentages) <- paste(drift_percentages, "%")
  }

  PEC_sw_drift <- PEC_sw_overspray * drift_percentages / 100
  return(PEC_sw_drift)
}
