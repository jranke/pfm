# Copyright (C) 2015,2016,2018  Johannes Ranke
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

# Register global variables
if(getRversion() >= '2.15.1') utils::globalVariables(c("destination", "study_type", "TP_identifier",
                                                       "soil_scenario_data_EFSA_2015"))

#' Calculate predicted environmental concentrations in soil
#'
#' This is a basic calculation of a contaminant concentration in bulk soil
#' based on complete, instantaneous mixing. If an interval is given, an 
#' attempt is made at calculating a long term maximum concentration using
#' the concepts layed out for example in the PPR panel opinion (EFSA 2012).
#' 
#' This assumes that the complete load to soil during the time specified by
#' 'interval' (typically 365 days) is dosed at once. As in the PPR panel
#' opinion cited below (PPR panel 2012), only temperature correction using the
#' Arrhenius equation is performed.  
#' 
#' Total soil and porewater PEC values for the scenarios as defined in the EFSA
#' guidance (2015, p. 13) can easily be calculated.
#' 
#' @note If temperature information is available in the selected scenarios, as
#'   e.g. in the EFSA scenarios, the DT50 for groundwater modelling
#'   (destination 'PECgw') is taken from the chent object, otherwise the DT50
#'   with destination 'PECsoil'.
#' @importFrom methods is
#' @param rate Application rate in units specified below
#' @param rate_units Defaults to g/ha
#' @param interception The fraction of the application rate that does not reach the soil
#' @param mixing_depth Mixing depth in cm
#' @param interval Period of the deeper mixing, defaults to 365, which is a year if
#'   degradation rate units are in days
#' @param n_periods Number of periods to be considered for long term PEC calculations
#' @param PEC_units Requested units for the calculated PEC. Only mg/kg currently supported
#' @param PEC_pw_units Only mg/L currently supported
#' @param tillage_depth Periodic (see interval) deeper mixing in cm
#' @param chent An optional chent object holding substance specific information. Can 
#'   also be a name for the substance as a character string
#' @param DT50 If specified, overrides soil DT50 endpoints from a chent object
#'   If DT50 is not specified here and not available from the chent object, zero
#'   degradation is assumed
#' @param Koc If specified, overrides Koc endpoints from a chent object
#' @param Kom Calculated from Koc by default, but can explicitly be specified
#'   as Kom here
#' @param t_avg Averaging times for time weighted average concentrations
#' @param scenarios If this is 'default', the DT50 will be used without correction
#'   and soil properties as specified in the REACH guidance (R.16, Table
#'   R.16-9) are used for porewater PEC calculations.  If this is "EFSA_2015",
#'   the DT50 is taken to be a modelling half-life at 20°C and pF2 (for when
#'   'chents' is specified, the DegT50 with destination 'PECgw' will be used),
#'   and corrected using an Arrhenius activation energy of 65.4 kJ/mol. Also
#'   model and scenario adjustment factors from the EFSA guidance are used.
#' @param porewater Should equilibrium porewater concentrations be estimated
#'   based on Kom and the organic carbon fraction of the soil instead of total
#'   soil concentrations?  Based on equation (7) given in the PPR panel opinion
#'   (EFSA 2012, p. 24) and the scenarios specified in the EFSA guidance (2015,
#'   p. 13).
#' @return The predicted concentration in soil
#' @references EFSA Panel on Plant Protection Products and their Residues (2012)
#'   Scientific Opinion on the science behind the guidance for scenario
#'   selection and scenario parameterisation for predicting environmental
#'   concentrations of plant protection products in soil. \emph{EFSA Journal}
#'   \bold{10}(2) 2562, doi:10.2903/j.efsa.2012.2562
#' 
#'   EFSA (European Food Safety Authority) (2015) EFSA guidance document for
#'   predicting environmental concentrations of active substances of plant
#'   protection products and transformation products of these active substances
#'   in soil. \emph{EFSA Journal} \bold{13}(4) 4093
#'   doi:10.2903/j.efsa.2015.4093
#' @author Johannes Ranke
#' @export
#' @examples
#' PEC_soil(100, interception = 0.25)
#' 
#' # This is example 1 starting at p. 79 of the EFSA guidance (2015)
#' PEC_soil(1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
#'                scenarios = "EFSA_2015")
#' PEC_soil(1000, interval = 365, DT50 = 250, t_av = c(0, 21),
#'                Kom = 1000, scenarios = "EFSA_2015", porewater = TRUE)
#' 
#' # The following is from example 4 starting at p. 85 of the EFSA guidance (2015)
#' # Metabolite M2
#' # Calculate total and porewater soil concentrations for tier 1 scenarios
#' # Relative molar mass is 100/300, formation fraction is 0.7 * 1
#' results_pfm <- PEC_soil(100/300 * 0.7 * 1 * 1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
#'                         scenarios = "EFSA_2015")
#' results_pfm_pw <- PEC_soil(100/300 * 0.7 * 1000, interval = 365, DT50 = 250, t_av = c(0, 21),
#'                            Kom = 100, scenarios = "EFSA_2015", porewater = TRUE)

PEC_soil <- function(rate, rate_units = "g/ha", interception = 0,
                     mixing_depth = 5, 
                     PEC_units = "mg/kg", PEC_pw_units = "mg/L",
                     interval = NA, n_periods = Inf,
                     tillage_depth = 20, 
                     chent = NA, 
                     DT50 = NA, 
                     Koc = NA, Kom = Koc / 1.724,
                     t_avg = 0,
                     scenarios = c("default", "EFSA_2015"),
                     porewater = FALSE)
{
  rate_to_soil = (1 - interception) * rate
  rate_units = match.arg(rate_units)
  PEC_units = match.arg(PEC_units)
  scenarios = match.arg(scenarios)
  sce <- switch(scenarios, 
    default = data.frame(rho = 1.5, T_arr = NA, theta_fc = 0.2, f_om = 1.724 * 0.02,
                         f_sce = 1, f_mod = 1, row.names = "default"),
    EFSA_2015 = if (porewater) soil_scenario_data_EFSA_2015[4:6, ] 
                else soil_scenario_data_EFSA_2015[1:3, ]
  )
  n_sce = nrow(sce)

  soil_volume = 100 * 100 * (mixing_depth/100)   # in m3
  soil_mass = soil_volume * sce$rho * 1000  # in kg

  # The following is C_T,ini from EFSA 2012, p. 22, but potentially with interception > 0
  PEC_soil_ini = rate_to_soil * 1000 / soil_mass     # in mg/kg

  # Decide which DT50 to take, or set degradation to zero if no DT50 available
  if (is.na(DT50) & is(chent, "chent")) {
    if (all(is.na(sce$T_arr))) {   # No temperature correction
      DT50 <- subset(chent$soil_degradation_endpoints, destination == "PECsoil")$DT50
    } else {
      DT50 <- subset(chent$soil_degradation_endpoints, destination == "PECgw")$DT50
    }
    if (length(DT50) > 1) stop("More than one PECsoil DT50 in chent object")
    if (length(DT50) == 0) DT50 <- Inf
  }
  k = log(2)/DT50

  # Temperature correction of degradation (accumulation)
  if (all(is.na(sce$T_arr))) {   # No temperature correction
    f_T = 1
  } else {
    # Temperature correction as in EFSA 2012 p. 23
    f_T = ifelse(sce$T_arr == 0, 
                 0, 
                 exp(- (65.4 / 0.008314) * (1/(sce$T_arr + 273.15) - 1/293.15)))
  }

  # X is the fraction left after one period (EFSA guidance p. 23)
  X = exp(- k * f_T * interval)
  
  # f_accu is the fraction left after n periods (X + X^2 + ...)
  f_accu = 0 
  if (!is.na(interval)) {
    if (n_periods == Inf) {
      f_accu = X/(1 - X)
    } else {
      for (i in 1:n_periods) {
        f_accu = f_accu + X^i
      }
    }
  }

  f_tillage = mixing_depth / tillage_depth

  PEC_background = f_accu * f_tillage * PEC_soil_ini

  PEC_soil = (1 + f_accu * f_tillage) * PEC_soil_ini

  # Get porewater PEC if requested
  if (porewater) {

    # If Kom is not specified, try to get K(f)oc
    if (is.na(Kom)) {
      # If Koc not specified, try to get K(f)oc from chent
      if (is.na(Koc) & is(chent, "chent")) {
        Koc <- soil_Kfoc(chent)
      } 
      Kom <- Koc / 1.724
    }

    if (is.na(Kom)) stop("No Kom information specified")

    PEC_soil = PEC_soil/((sce$theta_fc/sce$rho) + sce$f_om * Kom)
  }

  # Scenario adjustment factors
  PEC_soil_sce = PEC_soil * sce$f_sce

  # Model adjustment factors
  PEC_soil_sce_mod = PEC_soil_sce * sce$f_mod

  result <- matrix(NA, ncol = n_sce, nrow = length(t_avg),
                   dimnames = list(t_avg = t_avg, scenario = rownames(sce)))
  
  result[1, ] <- PEC_soil_sce_mod

  for (i in seq_along(t_avg)) {
    t_av_i <- t_avg[i]
    if (t_av_i > 0) {
      # Equation 10 from p. 24 (EFSA 2015)
      result[i, ] <- PEC_soil_sce_mod/(t_av_i * f_T * k) * (1 - exp(- f_T * k * t_av_i))
    }
  }

  return(result)
}
