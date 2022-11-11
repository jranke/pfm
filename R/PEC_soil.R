# Register global variables
if(getRversion() >= '2.15.1') utils::globalVariables(c("destination", "study_type", "TP_identifier",
                                                       "soil_scenario_data_EFSA_2015",
                                                       "soil_scenario_data_EFSA_2017", "bottom"))

#' Calculate predicted environmental concentrations in soil
#'
#' This is a basic calculation of a contaminant concentration in bulk soil
#' based on complete, instantaneous mixing. If an interval is given, an
#' attempt is made at calculating a long term maximum concentration using
#' the concepts layed out in the PPR panel opinion (EFSA PPR panel 2012
#' and in the EFSA guidance on PEC soil calculations (EFSA, 2015, 2017).
#'
#' This assumes that the complete load to soil during the time specified by
#' 'interval' (typically 365 days) is dosed at once. As in the PPR panel
#' opinion cited below (EFSA PPR panel 2012), only temperature correction using the
#' Arrhenius equation is performed.
#'
#' Total soil and porewater PEC values for the scenarios as defined in the EFSA
#' guidance (2017, p. 14/15) can easily be calculated.
#' @note While time weighted average (TWA) concentrations given in the examples
#' from the EFSA guidance from 2015 (p. 80) are be reproduced, this is not
#' true for the TWA concentrations given for the same example in the EFSA guidance
#' from 2017 (p. 92).
#' @note According to the EFSA guidance (EFSA, 2017, p. 43), leaching should be
#'   taken into account for the EFSA 2017 scenarios, using the evaluation depth
#'   (here mixing depth) as the depth of the layer from which leaching takes
#'   place.  However, as the amount leaching below the evaluation depth
#'   (often 5 cm) will partly be mixed back during tillage, the default in this function
#'   is to use the tillage depth for the calculation of the leaching rate.
#' @note If temperature information is available in the selected scenarios, as
#'   e.g. in the EFSA scenarios, the DT50 for groundwater modelling
#'   (destination 'PECgw') is taken from the chent object, otherwise the DT50
#'   with destination 'PECsoil'.
#' @importFrom methods is
#' @param rate Application rate in units specified below
#' @param rate_units Defaults to g/ha
#' @param interception The fraction of the application rate that does not reach the soil
#' @param mixing_depth Mixing depth in cm
#' @param interval Period of the deeper mixing. The default is NA, i.e. no
#'   deeper mixing.  For annual deeper mixing, set this to 365 when degradation
#'   units are in days
#' @param n_periods Number of periods to be considered for long term PEC calculations
#' @param PEC_units Requested units for the calculated PEC. Only mg/kg currently supported
#' @param PEC_pw_units Only mg/L currently supported
#' @param tillage_depth Periodic (see interval) deeper mixing in cm
#' @param leaching_depth EFSA (2017) uses the mixing depth (ecotoxicological
#'   evaluation depth) to calculate leaching for annual crops where tillage
#'   takes place. By default, losses from the layer down to the tillage
#'   depth are taken into account in this implementation.
#' @param cultivation Does mechanical cultivation in the sense of EFSA (2017)
#'   take place, i.e. twice a year to a depth of 5 cm? Ignored for scenarios
#'   other than EFSA_2017
#' @param crop Ignored for scenarios other than EFSA_2017. Only annual crops
#' are supported when these scenarios are used. Only crops with a single cropping
#' cycle per year are currently supported.
#' @param chent An optional chent object holding substance specific information. Can
#'   also be a name for the substance as a character string
#' @param DT50 If specified, overrides soil DT50 endpoints from a chent object
#'   If DT50 is not specified here and not available from the chent object, zero
#'   degradation is assumed
#' @param FOMC If specified, it should be a named numeric vector containing
#'  the FOMC parameters alpha and beta. This overrides any other degradation
#'  endpoints, and the degradation during the interval and after the maximum PEC
#'  is calculated using these parameters without temperature correction
#' @param Koc If specified, overrides Koc endpoints from a chent object
#' @param Kom Calculated from Koc by default, but can explicitly be specified
#'   as Kom here
#' @param t_avg Averaging times for time weighted average concentrations
#' @param t_act Time series for actual concentrations
#' @param scenarios If this is 'default', the DT50 will be used without correction
#'   and soil properties as specified in the REACH guidance (R.16, Table
#'   R.16-9) are used for porewater PEC calculations.  If this is "EFSA_2015",
#'   the DT50 is taken to be a modelling half-life at 20°C and pF2 (for when
#'   'chent' is specified, the DegT50 with destination 'PECgw' will be used),
#'   and corrected using an Arrhenius activation energy of 65.4 kJ/mol. Also
#'   model and scenario adjustment factors from the EFSA guidance are used.
#' @param leaching Should leaching be taken into account? The default is FALSE,
#'   except when the EFSA_2017 scenarios are used.
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
#'   EFSA (European Food Safety Authority) 2017) EFSA guidance document for
#'   predicting environmental concentrations of active substances of plant
#'   protection products and transformation products of these active substances
#'   in soil. \emph{EFSA Journal} \bold{15}(10) 4982
#'   doi:10.2903/j.efsa.2017.4982
#'
#'   EFSA (European Food Safety Authority) (2015) EFSA guidance document for
#'   predicting environmental concentrations of active substances of plant
#'   protection products and transformation products of these active substances
#'   in soil. \emph{EFSA Journal} \bold{13}(4) 4093
#'   doi:10.2903/j.efsa.2015.4093
#'
#' @author Johannes Ranke
#' @export
#' @examples
#' PEC_soil(100, interception = 0.25)
#'
#' # This is example 1 starting at p. 92 of the EFSA guidance (2017)
#' # Note that TWA concentrations differ from the ones given in the guidance
#' # for an unknown reason (the values from EFSA (2015) can be reproduced).
#' PEC_soil(1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
#'                Kom = 1000, scenarios = "EFSA_2017")
#' PEC_soil(1000, interval = 365, DT50 = 250, t_av = c(0, 21),
#'                Kom = 1000, scenarios = "EFSA_2017", porewater = TRUE)
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
                     leaching_depth = tillage_depth,
                     crop = "annual",
                     cultivation = FALSE,
                     chent = NA,
                     DT50 = NA,
                     FOMC = NA,
                     Koc = NA, Kom = Koc / 1.724,
                     t_avg = 0,
                     t_act = NULL,
                     scenarios = c("default", "EFSA_2017", "EFSA_2015"),
                     leaching = scenarios == "EFSA_2017",
                     porewater = FALSE)
{
  # Comments with equation numbers in parentheses refer to
  # the numbering in the EFSA guidance from 2017, appendix A
  rate_to_soil = (1 - interception) * rate
  rate_units = match.arg(rate_units)
  PEC_units = match.arg(PEC_units)
  scenarios = match.arg(scenarios)
  if (scenarios == "EFSA_2017") {
    if (crop != "annual") stop("Only annual crops are currently supported")
    if (cultivation) stop("Permanent crops with mechanical cultivation are currently not supported")
  }
  sce <- switch(scenarios,
    default = data.frame(rho = 1.5, T_arr = NA, theta_fc = 0.2, f_om = 1.724 * 0.02,
                         f_sce = 1, f_mod = 1, row.names = "default"),
    EFSA_2015 = if (porewater) soil_scenario_data_EFSA_2015[4:6, ]
                else soil_scenario_data_EFSA_2015[1:3, ],
    EFSA_2017 = if (porewater) soil_scenario_data_EFSA_2017[4:6, ]
                else soil_scenario_data_EFSA_2017[1:3, ]
  )
  n_sce = nrow(sce)

  soil_volume = 100 * 100 * (mixing_depth/100)   # in m3
  soil_mass = soil_volume * sce$rho * 1000  # in kg

  # In EFSA (2017), f_om is depth dependent for permanent crops
  # For annual crops, the correction factor is 1 (uniform f_om is
  # assumed)
  mixing_depth_string <- paste(mixing_depth, "cm")
  tillage_depth_string <- paste(tillage_depth, "cm")
  if (scenarios == "EFSA_2017" & crop != "annual") {
    # Correction factors f_f_om with depth according to EFSA 2017, p. 15
    f_f_om_depth = data.frame(
      depth = c("0-5", "5-10", "10-20", "20-30"),
      bottom = c(5, 10, 20, 30),
      thickness = c(5, 5, 10, 10),
      f_f_om_no_cultivation = c(1.95, 1.30, 0.76, 0.62),
      f_f_om_cultivation = c(1.50, 1.20, 0.90, 0.75))
    # Averages for the 0-5 cm and 0-20 cm layers
    f_f_om_layer = data.frame(
      layer = c("0-5", "0-20"),
      f_f_om_no_cultivation = c(1.95, (5 * 1.95 + 5 * 1.3 + 10 * 0.76)/20),
      f_f_om_cultivation = c(1.50, (5 * 1.5 + 5 * 1.2 + 10 * 0.9)/20))
    # The resulting mean value for 0-20 cm and no cultivation of 1.1925 is
    # consistent with the value of 1.19 given in Table B.4 on p. 54 of the
    # 2017 EFSA guidance

    f_f_om_average <- function(depth, cultivation) {
      rownames(f_f_om_layer) = paste(f_f_om_layer$layer, "cm")
      if (depth %in% c(5, 20)) {
        if (cultivation) {
          return(f_f_om_layer[paste0("0-", depth, " cm"), "f_f_om_cultivation"])
        } else {
          return(f_f_om_layer[paste0("0-", depth, " cm"), "f_f_om_no_cultivation"])
        }
      } else {
        stop("Depths other than 5 and 20 cm are not supported when using EFSA 2017 scenarios for permanent crops")
      }
    }

    # For the loss via leaching, the equilibrium and therefore the f_om at the
    # bottom of the layer is probably most relevant. Unfortunately this is not
    # clarified in the guidance.
    f_f_om_bottom <- function(depth, cultivation) {
      bottom_depth <- depth # rename to avoid confusion when subsetting
      if (cultivation) {
        f_f_om <- subset(f_f_om_depth, bottom == bottom_depth)$f_f_om_cultivation
      } else {
        f_f_om <- subset(f_f_om_depth, bottom == bottom_depth)$f_f_om_no_cultivation
      }
      return(f_f_om)
    }
  } else {
    f_f_om_average <- f_f_om_bottom <- function(depth, cultivation) 1
  }

  # The following is C_T,ini from EFSA 2012, p. 22, but potentially with interception > 0
  PEC_soil_ini = rate_to_soil * 1000 / soil_mass     # in mg/kg (A1)

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
  k_ref = log(2)/DT50 # (A5)

  # Temperature correction of degradation (accumulation)
  if (all(is.na(sce$T_arr))) {   # No temperature correction
    f_T = 1
  } else {
    # Temperature correction as in EFSA 2012 p. 23
    f_T = ifelse(sce$T_arr == 0,
                 0, # (A4b)
                 exp(- (65.4 / 0.008314) * (1/(sce$T_arr + 273.15) - 1/293.15))) # (A4a)
  }

  # Define Kom if needed
  if (leaching | porewater) {
    # If Kom is not specified, try to get K(f)oc
    if (is.na(Kom)) {
      # If Koc not specified, try to get K(f)oc from chent
      if (is.na(Koc) & is(chent, "chent")) {
        Koc <- soil_Kfoc(chent)
      } else {
        stop("No Kom information specified")
      }
      Kom <- Koc / 1.724
    }
  }

  if (leaching) {
    leaching_depth_string <- paste(leaching_depth, "cm")
    f_q <- c("1 cm" = 0.8, "2.5 cm" = 0.75, "5 cm" = 0.7, "20 cm" = 0.5) # EFSA 2017 p. 54
    if (leaching_depth_string %in% names(f_q)) {
      q_mm_year = f_q[leaching_depth_string] * sce$prec # Irrigation at tier 1? I have not found values for Tier 1
      q_dm_day = q_mm_year / (100 * 365)
      leaching_depth_dm <- leaching_depth / 10

      k_leach = q_dm_day/(leaching_depth_dm * (sce$theta_fc + sce$rho * f_f_om_average(leaching_depth, cultivation) * sce$f_om * Kom))
    } else {
      stop("Leaching can not be calculated, because f_q for this leaching depth is undefined")
    }
  } else {
    k_leach = 0
  }

  # X is the fraction left after one period (EFSA 2017 guidance p. 23)
  X = exp(- (k_ref * f_T + k_leach) * interval) # (A3)

  # f_accu is the fraction left after n periods (X + X^2 + ...)
  f_accu = 0
  if (!is.na(interval)) {
    if (n_periods == Inf) {
      if (!is.na(FOMC[1])) {
        f_accu = get_vertex(x = 8:10, y = PEC_FOMC_accu_rel(n = 10, interval, FOMC)[8:10])$yv
        warning("The long term calculation is based on a pseudo-plateau constructed\n  ",
                "by fitting a parabola through the residues after 8, 9 and 10 years.\n  ",
                "This is the method used by the ESCAPE software tool for separate consideration of residues.\n  ",
                "Please check the validity by specifying e.g. n_periods = 20 or 50.")
      } else {
        f_accu = X/(1 - X) # part of (A2)
      }
    } else {
      for (i in 1:n_periods) {
        if (!is.na(FOMC[1])) {
          f_accu = f_accu + 1 / (((i * interval)/FOMC[["beta"]]) + 1)^FOMC[["alpha"]]
        } else {
          f_accu = f_accu + X^i
        }
      }
    }
  }

  f_tillage = mixing_depth / tillage_depth

  PEC_background = f_accu * f_tillage * PEC_soil_ini # (A2)

  PEC_soil = PEC_soil_ini + PEC_background # (A6)

  # Get porewater PEC if requested
  if (porewater) {

    PEC_soil = PEC_soil/((sce$theta_fc/sce$rho) + f_f_om_average(mixing_depth, cultivation) * sce$f_om * Kom) # (A7)
  }

  # Scenario adjustment factors
  PEC_soil_sce = PEC_soil * sce$f_sce

  # Model adjustment factors
  PEC_soil_sce_mod = PEC_soil_sce * sce$f_mod

  if (is.null(t_act)) {
    result <- matrix(NA, ncol = n_sce, nrow = length(t_avg),
                     dimnames = list(t_avg = t_avg, scenario = rownames(sce)))

    result[1, ] <- PEC_soil_sce_mod

    for (i in seq_along(t_avg)) {
      t_av_i <- t_avg[i]
      k_avg <- f_T * k_ref # Leaching not taken into account, EFSA 2017 p. 43
      if (t_av_i > 0) {
        # Equation 10 from p. 24 (EFSA 2015)
        if (!is.na(FOMC[1])) {
          result[i, ] <- PEC_soil_sce_mod * (FOMC[["beta"]])/(t_av_i * (1 - FOMC[["alpha"]])) *
            ((t_av_i/FOMC[["beta"]] + 1)^(1 - FOMC[["alpha"]]) - 1)
        } else {
          result[i, ] <- PEC_soil_sce_mod/(t_av_i * k_avg) * (1 - exp(- k_avg * t_av_i)) # (A8)
        }
      }
    }
  } else {
    if (!identical(t_avg, 0)) stop("Either t_act or t_avg can be specified")
    result <- matrix(NA, ncol = n_sce, nrow = length(t_act),
                     dimnames = list(t_act = t_act, scenario = rownames(sce)))

    result[1, ] <- PEC_soil_sce_mod

    for (i in seq_along(t_act)) {
      t_ac_i <- t_act[i]
      k_act <- f_T * k_ref # Leaching not taken into account
      if (t_ac_i > 0) {
        # Equation 10 from p. 24 (EFSA 2015)
        if (!is.na(FOMC[1])) {
          result[i, ] <-
            PEC_soil_sce_mod / ((t_ac_i/FOMC[["beta"]] + 1)^(FOMC[["alpha"]]))
        } else {
          result[i, ] <- PEC_soil_sce_mod * exp(- k_act * t_ac_i)
        }
      }
    }

  }

  return(result)
}

#' Get the relative accumulation of an FOMC model over multiples of an interval
#' @param n number of applications
#' @param interval Time between applications
#' @param FOMC Named numeric vector containing the FOMC parameters alpha and beta
#' @return A numeric vector containing all n accumulation factors for the n applications
#' @export
PEC_FOMC_accu_rel <- function(n, interval, FOMC) {
  PEC_accu_rel <- 0
  for (i in 1:(n - 1)) {
    PEC_accu_rel[i + 1] <- PEC_accu_rel[i] + mkin::FOMC.solution(i * interval, 1,
                                                         FOMC[["alpha"]], FOMC[["beta"]])
  }
  return(PEC_accu_rel)
}

#' Fit a parabola through three points
#'
#' This was inspired by an answer on stackoverflow
#' https://stackoverflow.com/a/717791
#' @param x Three x coordinates
#' @param y Three y coordinates
get_vertex <- function(x, y) {
  m <- cbind(x^2, x, 1)
  m_inv <- solve(m)
  res <- m_inv %*% y
  A <- res[1]
  B <- res[2]
  C <- res[3]
  xv = -B / (2*A)
  yv = C - B**2 / (4*A)
  return(list(xv = xv, yv = yv, A = A, B = B, C = C))
}

#' Calculate initial and accumulation PEC soil for a set of metabolites
#'
#' @param rate Application rate in units specified below
#' @param mw_parent The molecular weight of the parent compound
#' @param mets A dataframe with metabolite identifiers as rownames
#'   and columns "mw", "occ" and "DT50" holding their molecular weight,
#'   maximum occurrence in soil and their soil DT50
#' @param interval The interval for accumulation calculations
#' @param ... Further arguments are passed to PEC_soil
#' @export
PEC_soil_mets <- function(rate, mw_parent, mets, interval = 365, ...)
{
  result <- matrix(NA, nrow = nrow(mets), ncol = 3,
                  dimnames = list(mets = rownames(mets),
                                  PECs = c("Initial",
                                           "Plateau",
                                           "Accumulation")))
  result[, "Initial"] <- sapply(rownames(mets),
    function(x) {
      PEC_soil(mets[x, "occ"] * (mets[x, "mw"] / mw_parent) * rate,
        interval = NA, ...)
    }
  )

  result[, "Accumulation"] <- sapply(rownames(mets),
    function(x) {
      PEC_soil(mets[x, "occ"] * (mets[x, "mw"] / mw_parent) * rate,
        DT50 = mets[x, "DT50"],
        interval = interval, ...)
    }
  )

  result[, "Plateau"] <- result[, "Accumulation"] -
    result[, "Initial"]

  return(result)
}
