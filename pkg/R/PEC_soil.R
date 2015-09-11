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

if(getRversion() >= '2.15.1') utils::globalVariables(c("destination", "study_type", "TP_identifier"))

#' Calculate predicted environmental concentrations in soil for a product
#'
#' Calculates long term accumulation PEC values
#'
#' @param product An object of class pp
#' @param rate Application rate in units specified below
#' @param rate_units Defaults to g/ha
#' @param interception The fraction of the application rate that does not reach the soil
#' @param mixing_depth Mixing depth in cm
#' @param tillage_depth Periodic (see interval) deeper mixing in cm
#' @param interval Period of the deeper mixing, defaults to 365, which is a year if
#'   rate units are in days
#' @param bulk_density Bulk density of the soil. Defaults to 1.5 kg/L, or 1500 kg/m3
#' @param PEC_units Requested units for the calculated PEC. Only mg/kg currently supported
#' @return A data frame with compound names, and initial, plateau maximum, plateau minimum (background)
#'   and long term maximum predicted concentrations in soil
#' @export PEC_soil_product
#' @author Johannes Ranke
PEC_soil_product <- function(product, rate, rate_units = "L/ha", interception = 0,
                             mixing_depth = 5, tillage_depth = 20, 
                             interval = 365,
                             bulk_density = 1.5,
                             PEC_units = "mg/kg") {
  rate_units = match.arg(rate_units)
  PEC_units = match.arg(PEC_units)
  if (product$density_units != "g/L") stop("Product density other than g/L not supported")
  if (product$concentration_units != "g/L") {
    stop("Active ingredient concentration units other than g/L not supported")
  }

  results <- data.frame(compound = character(0), initial = numeric(0), 
                        plateau_max = numeric(0), plateau_min = numeric(0), 
                        long_term_max = numeric(0),
                        stringsAsFactors = FALSE)

  for (ai_name in names(product$ais)) {
    ai <- product$ais[[ai_name]]
    ai_rate <- rate * product$concentrations[ai_name] 
    ini <- PEC_soil(ai_rate,
                    interception = interception, mixing_depth = mixing_depth,
                    bulk_density = bulk_density)
    results[ai_name, "compound"] <- ai$identifier
    results[ai_name, "initial"] <- ini

    ini_tillage <- ini * mixing_depth / tillage_depth
    DT50 <- subset(ai$soil_degradation_endpoints, destination == "PECsoil")$DT50
    if (length(DT50) > 1) stop("More than one PECsoil DT50 for", ai_name)
    if (length(DT50) > 0) {
      if (!is.na(DT50)) {
        k <- log(2) / DT50
        plateau_max <- ini_tillage / (1 - exp( - k * interval))
        plateau_min <- plateau_max *  exp( - k * interval)
        long_term_max <- plateau_min + ini
        results[ai_name, c("plateau_max", "plateau_min", "long_term_max")] <- 
          c(plateau_max, plateau_min, long_term_max)
      }
    }

    for (TP_name in names(ai$TPs)) {
      TP <- ai$TPs[[TP_name]]
      max_occurrence = max(subset(ai$transformations, 
                                  grepl("soil", study_type) & 
                                  TP_identifier == TP$identifier, max_occurrence))
      TP_rate <- ai_rate * TP$mw / ai$mw * max_occurrence
      ini <- PEC_soil(TP_rate, interception = interception, mixing_depth = mixing_depth,
                      bulk_density = bulk_density)
      results[TP_name, "compound"] <- TP$identifier
      results[TP_name, "initial"] <- ini

      ini_tillage <- ini * mixing_depth / tillage_depth
      DT50 <- subset(TP$soil_degradation_endpoints, destination == "PECsoil")$DT50
      if (length(DT50) > 1) stop("More than one PECsoil DT50 for", TP_name)
      if (length(DT50) > 0) {
        if (!is.na(DT50)) {
          k <- log(2) / DT50
          plateau_max <- ini_tillage / (1 - exp( - k * interval))
          plateau_min <- plateau_max *  exp( - k * interval)
          results[TP_name, c("plateau_max", "plateau_min")] <- c(plateau_max, plateau_min)
          long_term_max <- plateau_min + ini
          results[TP_name, c("plateau_max", "plateau_min", "long_term_max")] <- 
            c(plateau_max, plateau_min, long_term_max)
        }
      }
    }
  }
  return(results)
}

