#' Calculate FOCUS Step 1 PEC surface water
#'
#' This is an attempt to reimplement the FOCUS Step 1 and 2 calculator authored
#' by Michael Klein. The Step 1 and 2 scenario assumes an area ratio of 10:1
#' between field and waterbody, and a water depth of 30 cm.
#' I did not (yet) implement the TWA formulas for times later than day 1, as I
#' did not understand them right away.
#' Also, Step 2 is not implemented (yet).
#'
#' @export
#' @param parent A list containing substance specific parameters
#' @param rate The application rate in g/ha. Overriden when
#'   applications are given explicitly
#' @param n The number of applications
#' @param i The application interval
#' @param applications A dataframe containing times and amounts of each application
#' @param step At the moment, only Step 1 is implemented
#' @examples
#' dummy_1 <- chent_focus_sw(cwsat = 6000, DT50_ws = 6, Koc = 344.8)
#' PEC_sw_focus(dummy_1, 3000, f_drift = 0)
PEC_sw_focus <- function(parent, rate, n = 1, i = NA,
  applications = data.frame(time = seq(0, 0 + n * i, length.out = n),
                            amount = rate),
  met = NULL,
  step = 1,
  f_drift = 0.02759, f_rd = 0.1)
{
  if (is.null(met)) {
    mw_ratio = 1
    max_soil = 1
    max_ws = 1
    Koc = parent$Koc
    DT50_ws = parent$DT50_ws
  } else {
    mw_ratio = met$mw / parent$mw
    max_soil = met$max_soil
    max_ws = met$max_ws
    Koc = met$Koc
    DT50_ws = met$DT50_ws
  }

  # Rates for a single application
  eq_rate_drift_s = mw_ratio * max_ws * rate
  eq_rate_rd_s = mw_ratio * max_soil * rate

  # Drift input
  input_drift_s = f_drift * eq_rate_drift_s / 10 # mg/m2
  input_drift = n * input_drift_s

  # Runoff/drainage input
  ratio_field_wb = 10 # 10 m2 of field for each m2 of the waterbody
  input_rd_s = f_rd * eq_rate_rd_s * ratio_field_wb / 10
  input_rd = n * input_rd_s

  # Fraction of compound entering the water phase via runoff/drainage
  depth_sw  = 0.3  # m
  depth_sed = 0.05 # m
  depth_sed_eff = 0.01 # m, only relevant for sorption
  rho_sed = 0.8 # kg/L
  f_OC = 0.05 # 5% organic carbon in sediment
  f_rd_sw = depth_sw / (depth_sw + (depth_sed_eff * rho_sed * f_OC * Koc))
  f_rd_sed = 1 - f_rd_sw

  # Initial PECs
  PEC_sw_0 = (input_drift + input_rd * f_rd_sw) / depth_sw        # µg/L
  PEC_sed_0 = (input_rd * f_rd_sed) / (depth_sed * rho_sed)       # µg/kg

  # Initial PECs when assuming partitioning also of drift input
  PEC_sw_0_part = (input_drift + input_rd) * f_rd_sw / depth_sw                  # µg/L
  PEC_sed_0_part = (input_drift + input_rd) * f_rd_sed / (depth_sed * rho_sed)   # µg/kg

  t_out = c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100)
  PEC = matrix(NA, nrow = length(t_out), ncol = 4,
               dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC["0", "PECsw"]  = PEC_sw_0
  PEC["0", "PECsed"] = PEC_sed_0

  # Degradation after partitioning according to Koc
  k_ws = log(2)/DT50_ws
  PEC[as.character(t_out[-1]), "PECsw"]  = PEC_sw_0_part  * exp( - k_ws * t_out[-1])
  PEC[as.character(t_out[-1]), "PECsed"] = PEC_sed_0_part * exp( - k_ws * t_out[-1])

  # TWA concentrations
  PEC["1", "TWAECsw"] = (PEC_sw_0 + PEC["1", "PECsw"]) / 2
  PEC["1", "TWAECsed"] = (PEC_sed_0 + PEC["1", "PECsed"]) / 2

  list(eq_rate_drift_s = eq_rate_drift_s,
       eq_rate_rd_s = eq_rate_rd_s,
       input_drift_s = input_drift_s,
       input_rd_s = input_rd_s,
       f_rd_sw = f_rd_sw, f_rd_sed = f_rd_sed,
       PEC = PEC)
}

#' Create an chemical compound object for FOCUS Step 1 and 2 calculations
#'
#' @export
#' @param cwsat Water solubility in mg/L
#' @param DT50_ws Half-life in water/sediment systems in days
#' @param Koc Partition coefficient between organic carbon and water
#'   in L/kg.
#' @return A list with the substance specific properties
chent_focus_sw <- function(Koc, DT50_ws, cwsat = 1000)
{
  list(Koc = Koc, DT50_ws = DT50_ws, cwsat = cwsat)
}


