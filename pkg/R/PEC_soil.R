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

#' Calculate predicted environmental concentrations in soil
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in bulk soil based on complete, instantaneous mixing.
#'
#' @param rate Application rate in units specified below
#' @param rate_units Defaults to g/ha
#' @param interception The fraction of the application rate that does not reach the soil
#' @param mixing_depth Mixing depth in cm
#' @param bulk_density Bulk density of the soil. Defaults to 1.5 kg/L, or 1500 kg/m3
#' @param PEC_units Requested units for the calculated PEC. Only mg/kg currently supported
#' @return The predicted concentration in soil
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_soil(100, interception = 0.25)
PEC_soil <- function(rate, rate_units = "g/ha", interception = 0,
                     mixing_depth = 5, bulk_density = 1.5,
                     PEC_units = "mg/kg")
{
  rate_to_soil = (1 - interception) * rate
  rate_units = match.arg(rate_units)
  PEC_units = match.arg(PEC_units)
  soil_volume = 100 * 100 * (mixing_depth/100)   # in m3
  soil_mass = soil_volume * bulk_density * 1000  # in kg
  PEC_soil = rate_to_soil * 1000 / soil_mass     # in mg/kg
  return(PEC_soil)
}
