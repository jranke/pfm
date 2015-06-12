# Copyright (C) 2015  Johannes Ranke
# Contact: jranke@uni-bremen.de
# This file is part of the R package pfm

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>

#' Calculate initial predicted environmental concentrations in sediment from
#' surface water concentrations
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
#' PEC_sw_sed(PEC_sw_drift_ini(100, distances = 1), percentage = 50)
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
