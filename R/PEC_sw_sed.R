#' Calculate predicted environmental concentrations in sediment from surface
#' water concentrations
#'
#' The method 'percentage' is equivalent to what is used in the CRD spreadsheet
#' PEC calculator
#'
#' @param PEC_sw Numeric vector or matrix of surface water concentrations in µg/L for
#'   which the corresponding sediment concentration is to be estimated
#' @param percentage The percentage in sediment, used for the percentage method
#' @param method The method used for the calculation
#' @param sediment_depth Depth of the sediment layer
#' @param water_depth Depth of the water body in cm
#' @param sediment_density The density of the sediment in kg/L (equivalent to
#'   g/cm3)
#' @param PEC_sed_units The units of the estimated sediment PEC value
#' @return The predicted concentration in sediment
#' @export
#' @author Johannes Ranke
#' @examples
#' library(pfm)
#' library(units)
#' PEC_sw_sed(PEC_sw_drift(100, distances = 1), percentage = 50)
PEC_sw_sed <- function(PEC_sw, percentage = 100, method = "percentage", 
                       sediment_depth = set_units(5, "cm"),
                       water_depth = set_units(30, "cm"),
                       sediment_density = set_units(1.3, "kg/L"),
                       PEC_sed_units = c("\u00B5g/kg", "mg/kg"))
{
  if (!inherits(PEC_sw, "units")) PEC_sw <- set_units(PEC_sw, "\u00B5g/L")
  if (!inherits(sediment_depth, "units")) PEC_sw <- set_units(sediment_depth, "cm")
  if (!inherits(water_depth, "units")) PEC_sw <- set_units(water_depth, "cm")
  if (!inherits(sediment_density, "units")) PEC_sw <- set_units(sediment_density, "cm")
  method <- match.arg(method)
  PEC_sed_units <- match.arg(PEC_sed_units)
  if (method == "percentage") {
    PEC_sed <- PEC_sw * (percentage/100) * as.numeric((water_depth / sediment_depth)) * (1 / sediment_density)
  }
  return(set_units(PEC_sed, PEC_sed_units, mode = "symbolic"))
}
