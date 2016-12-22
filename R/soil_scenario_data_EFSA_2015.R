#' Properties of the predefined scenarios from the EFSA guidance from 2015
#' 
#' Properties of the predefined scenarios used at Tier 1, Tier 2A and Tier 3A for the 
#' concentration in soil as given in the EFSA guidance (2015, p. 13/14). Also, the 
#' scenario and model adjustment factors from p. 15 and p. 17 are included.
#' 
#' @name soil_scenario_data_EFSA_2015
#' @docType data
#' @format A data frame with one row for each scenario. Row names are the scenario codes, 
#'   e.g. CTN for the Northern scenario for the total concentration in soil. Columns are 
#'   mostly self-explanatory. \code{rho} is the dry bulk density of the top soil.
#' @source EFSA (European Food Safety Authority) (2015)
#' EFSA guidance document for predicting environmental concentrations
#' of active substances of plant protection products and transformation products of these
#' active substances in soil. \emph{EFSA Journal} \bold{13}(4) 4093
#' doi:10.2903/j.efsa.2015.4093
#' @keywords datasets
#' @examples
#' \dontrun{
#'   # This is the code that was used to define the data
#'   soil_scenario_data_EFSA_2015 <- data.frame(
#'     Zone = rep(c("North", "Central", "South"), 2),
#'     Country = c("Estonia", "Germany", "France", "Denmark", "Czech Republik", "Spain"),
#'     T_arit = c(4.7, 8.0, 11.0, 8.2, 9.1, 12.8),
#'     T_arr = c(7.0, 10.1, 12.3, 9.8, 11.2, 14.7),
#'     Texture = c("Coarse", "Coarse", "Medium fine", "Medium", "Medium", "Medium"),
#'     f_om = c(0.118, 0.086, 0.048, 0.023, 0.018, 0.011),
#'     theta_fc = c(0.244, 0.244, 0.385, 0.347, 0.347, 0.347),
#'     rho = c(0.95, 1.05, 1.22, 1.39, 1.43, 1.51),
#'     f_sce = c(3, 2, 2, 2, 1.5, 1.5),
#'     f_mod = c(2, 2, 2, 4, 4, 4),
#'     stringsAsFactors = FALSE,
#'     row.names = c("CTN", "CTC", "CTS", "CLN", "CLC", "CLS")
#'   )
#'   save(soil_scenario_data_EFSA_2015, file = '../data/soil_scenario_data_EFSA_2015.RData')
#' }
#'
#' # And this is the resulting dataframe
#' soil_scenario_data_EFSA_2015
NULL