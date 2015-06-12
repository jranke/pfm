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

#' Calculate initial predicted environmental concentrations in surface water due to drainage using the UK method
#'
#' This implements the method specified in the UK data requirements handbook and was checked against the spreadsheet
#' published on the CRC website
#'
#' @param rate Application rate in g/ha
#' @param interception The fraction of the application rate that does not reach the soil
#' @param Koc The sorption coefficient normalised to organic carbon in L/kg
#' @param latest_application Latest application date, formatted as e.g. "01 July"
#' @param soil_DT50 Soil degradation half-life, if SFO kinetics are to be used
#' @param model The degradation model to be used. Either one of "FOMC", "DFOP", 
#'   "HS", or "IORE", or an mkinmod object
#' @param model_parms A named numeric vector containing the model parameters
#' @return The predicted concentration in surface water in µg/L
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_drainage_UK_ini(150, Koc = 100)
PEC_sw_drainage_UK_ini <- function(rate, interception = 0, Koc,
                                   latest_application = NULL, soil_DT50 = NULL,
                                   model = NULL, model_parms = NULL)
{
  percentage_lost <- SSLRC_mobility_classification(Koc)[[2]]
  amount_available <- rate * (1 - interception) # g/ha

  if (!missing(latest_application)) {
    lct <- Sys.getlocale("LC_TIME")
    tmp <- Sys.setlocale("LC_TIME", "C")
    latest <- as.Date(paste(latest_application, "1999"), "%d %b %Y")
    tmp <- Sys.setlocale("LC_TIME", lct)
    degradation_time <- as.numeric(difftime(as.Date("1999-10-01"), units = "days", latest))
    if (!missing(soil_DT50)) {
      k = log(2)/soil_DT50
      as.Date(paste(latest_application, "1999"), "%d %B %Y")

      amount_available <- amount_available * exp(-k * degradation_time)
      if (!missing(model)) stop("You already supplied a soil_DT50 value, implying SFO kinetics")
    }
    if (!missing(model)) {
      fraction_left <- pfm_degradation(model, parms = model_parms, 
                                       times = degradation_time)[1, "parent"]
      amount_available <- fraction_left * amount_available
    }
  } 

  volume = 130000 # L/ha
  PEC = 1e6 * (percentage_lost/100) * amount_available / volume
  return(PEC)
}
