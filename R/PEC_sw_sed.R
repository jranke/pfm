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
#' @param sediment_density The density of the sediment in L/kg (equivalent to
#'   g/cm3)
#' @param PEC_sed_units The units of the estimated sediment PEC value
#' @return The predicted concentration in sediment
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_sed(PEC_sw_drift(100, distances = 1), percentage = 50)
PEC_sw_sed <- function(PEC_sw, percentage = 100, method = "percentage", 
                       sediment_depth = 5, water_depth = 30,
                       sediment_density = 1.3,
                       PEC_sed_units = c("\u00B5g/kg", "mg/kg"))
{
  method = match.arg(method)
  PEC_sed_units = match.arg(PEC_sed_units)
  if (method == "percentage") {
    PEC_sed = PEC_sw * (percentage/100) * (water_depth / sediment_depth) * (1 / sediment_density)
    if (PEC_sed_units == "mg/kg") PEC_sed <- PEC_sed / 1000
  }
  return(PEC_sed)
}
