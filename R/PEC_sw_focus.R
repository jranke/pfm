#' Calculate FOCUS Step 1 PEC surface water
#'
#' This is reimplementation of Step 1 of the FOCUS Step 1 and 2 calculator
#' version 3.2, authored by Michael Klein. Note that results for multiple
#' applications should be compared to the corresponding results for a
#' single application. At current, this is not done automatically in
#' this implementation.
#'
#' @importFrom utils read.table
#' @references FOCUS (2014) Generic guidance for Surface Water Scenarios (version 1.4).
#'  FOrum for the Co-ordination of pesticde fate models and their USe.
#'  http://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/Generic%20FOCUS_SWS_vc1.4.pdf
#' @references Website of the Steps 1 and 2 calculator at the Joint Research
#'   Center of the European Union:
#'   http://esdac.jrc.ec.europa.eu/projects/stepsonetwo
#' @note The formulas for input to the waterbody via runoff/drainage of the
#'   parent and subsequent formation of the metabolite in water is not
#'   documented in the model description coming with the calculator
#' @note Step 2 is not implemented
#' @export
#' @param parent A list containing substance specific parameters
#' @param rate The application rate in g/ha. Overriden when
#'   applications are given explicitly
#' @param n The number of applications
#' @param i The application interval
#' @param met A list containing metabolite specific parameters. If not NULL, 
#'   the PEC is calculated for this compound, not the parent.
#' @param f_drift The fraction of the application rate reaching the waterbody
#'   via drift. If NA, this is derived from the scenario name and the number
#'   of applications via the drift data defined by the 
#'   \code{\link{FOCUS_Step_12_scenarios}}
#' @param f_rd The fraction of the amount applied reaching the waterbody via
#'   runoff/drainage. At Step 1, it is assumed to be 10%, be it the
#'   parent or a metabolite
#' @param scenario The name of the scenario. Must be one of the scenario
#'   names given in \code{\link{FOCUS_Step_12_scenarios}}
#' @examples
#' # Parent only
#' dummy_1 <- chent_focus_sw(cwsat = 6000, DT50_ws = 6, Koc = 344.8)
#' PEC_sw_focus(dummy_1, 3000, f_drift = 0)
#'
#' # Metabolite
#' new_dummy <- chent_focus_sw(mw = 250, Koc = 100)
#' M1 <- chent_focus_sw(mw = 100, cwsat = 100, DT50_ws = 100, Koc = 50, max_ws = 0, max_soil = 0.5)
#' PEC_sw_focus(new_dummy, 1000, scenario = "cereals, winter", met = M1)
PEC_sw_focus <- function(parent, rate, n = 1, i = NA,
  met = NULL,
  f_drift = NA, f_rd = 0.1,
  scenario = FOCUS_Step_12_scenarios$names)
{
  if (n > 1 & is.na(i)) stop("Please specify the interval i if n > 1")

  if (is.na(f_drift)) {
    scenario = match.arg(scenario)
    f_drift = FOCUS_Step_12_scenarios$drift[scenario, "1"] / 100
    # For Step 2 we would/will select the reduced percentiles for multiple apps:
    # if (n <= 8) {
    #  f_drift = FOCUS_Step_12_scenarios$drift[scenario, as.character(n)] / 100
    # } else {
    #   f_drift = FOCUS_Step_12_scenarios$drift[scenario, ">8"] / 100
    # }
  }

  if (is.null(met)) {
    cwsat = parent$cwsat
    mw_ratio = 1
    max_soil = 1
    max_ws = 1
    Koc = parent$Koc
    DT50_ws = parent$DT50_ws
  } else {
    cwsat = met$cwsat
    mw_ratio = met$mw / parent$mw
    max_soil = met$max_soil
    max_ws = met$max_ws
    Koc = met$Koc
    DT50_ws = met$DT50_ws
  }

  # Rates for a single application
  eq_rate_drift_s = mw_ratio * max_ws * rate
  # Parent only, or metabolite formed in soil:
  eq_rate_rd_s = mw_ratio * max_soil * rate
  # Metabolite formed in water (this part is not documented in the Help files
  # of the Steps 1/2 calculator):
  if (!is.null(met)) {
    eq_rate_rd_parent_s = mw_ratio * max_ws * rate
    eq_rate_rd_s_tot = eq_rate_rd_s + eq_rate_rd_parent_s
  } else {
    eq_rate_rd_parent_s = NA
    eq_rate_rd_s_tot = eq_rate_rd_s
  }

  # Drift input
  input_drift_s = f_drift * eq_rate_drift_s / 10 # mg/m2

  # Runoff/drainage input
  ratio_field_wb = 10 # 10 m2 of field for each m2 of the waterbody
  input_rd_s = f_rd * eq_rate_rd_s_tot * ratio_field_wb / 10
  input_rd = n * input_rd_s

  # No accumulation between multiple applications if 3 * DT50 < i
  if (n > 1 && (3 * DT50_ws < i)) {
    input_drift = input_drift_s
    input_rd = input_rd_s
  } else {
    input_drift = n * input_drift_s
    input_rd = n * input_rd_s
  }

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

  # Check if PEC_sw_max is above water solubility
  PEC_sw_max = max(PEC[, "PECsw"])
  if (PEC_sw_max > 1000 * cwsat) {
    warning("The maximum PEC surface water exceeds the water solubility")
  }

  PEC_sed_max = max(PEC[, "PECsed"])

  list(f_drift = f_drift,
    eq_rate_drift_s = eq_rate_drift_s,
    eq_rate_rd_s = eq_rate_rd_s,
    eq_rate_rd_parent_s = eq_rate_rd_parent_s,
    input_drift_s = input_drift_s,
    input_rd_s = input_rd_s,
    f_rd_sw = f_rd_sw, f_rd_sed = f_rd_sed,
    PEC = PEC,
    PEC_sw_max = PEC_sw_max,
    PEC_sed_max = PEC_sed_max
  )
}

#' Create a chemical compound object for FOCUS Step 1 calculations
#'
#' @export
#' @param cwsat Water solubility in mg/L
#' @param DT50_ws Half-life in water/sediment systems in days
#' @param Koc Partition coefficient between organic carbon and water
#'   in L/kg.
#' @param mw Molar weight in g/mol
#' @param max_soil Maximum observed fraction (dimensionless) in soil
#' @param max_ws Maximum observed fraction (dimensionless) in water/sediment
#'   systems
#' @return A list with the substance specific properties
chent_focus_sw <- function(Koc, DT50_ws = NA, cwsat = 1000, mw = NA, max_soil = 1, max_ws = 1)
{
  list(Koc = Koc, DT50_ws = DT50_ws, cwsat = cwsat,
       mw = mw, max_soil = max_soil, max_ws = max_ws)
}
