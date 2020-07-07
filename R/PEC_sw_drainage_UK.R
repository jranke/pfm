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
#' @param model The soil degradation model to be used. Either one of "FOMC",
#'   "DFOP", "HS", or "IORE", or an mkinmod object
#' @param model_parms A named numeric vector containing the model parameters
#' @return The predicted concentration in surface water in µg/L
#' @references HSE's Chemicals Regulation Division (CRD) Active substance
#'   PECsw calculations (for UK specific authorisation requests)
#'   \url{https://www.hse.gov.uk/pesticides/topics/pesticide-approvals/pesticides-registration/data-requirements-handbook/fate/active-substance-uk.htm}
#'   accessed 2019-09-27
#'
#'   Drainage PECs Version 1.0 (2015) Spreadsheet published at
#'   \url{https://www.hse.gov.uk/pesticides/topics/pesticide-approvals/pesticides-registration/data-requirements-handbook/fate/pec-tools-2015/PEC\%20sw-sed\%20(drainage).xlsx}
#'   accessed 2019-09-27
#' @export
#' @author Johannes Ranke
#' @examples
#' PEC_sw_drainage_UK(150, Koc = 100)
PEC_sw_drainage_UK <- function(rate, interception = 0, Koc,
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
