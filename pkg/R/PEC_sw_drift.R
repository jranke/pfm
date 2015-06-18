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

#' Calculate predicted environmental concentrations in surface water due to drift
#'
#' This is a basic, vectorised form of a simple calculation of a contaminant
#' concentration in surface water based on complete, instantaneous mixing
#' with input via spray drift.
#'
#' @param rate Application rate in units specified below
#' @param applications Number of applications for selection of drift percentile
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
  PEC_sw_drift <- PEC_sw_overspray * pfm::drift_data_JKI[[applications]][dist_index, crop] / 100
  names(PEC_sw_drift) <- paste(dist_index, "m")
  return(PEC_sw_drift)
}
