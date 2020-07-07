#' Determine the SSLRC mobility classification for a chemical substance from its Koc
#'
#' This implements the method specified in the UK data requirements handbook and was
#' checked against the spreadsheet published on the CRC website
#'
#' @param Koc The sorption coefficient normalised to organic carbon in L/kg
#' @return A list containing the classification and the percentage of the
#'   compound transported per 10 mm drain water
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
#' SSLRC_mobility_classification(100)
#' SSLRC_mobility_classification(10000)
SSLRC_mobility_classification <- function(Koc)
{
  if (!is.numeric(Koc) | length(Koc) != 1) stop("Please give a single number")
  if (is.na(Koc)) {
    result <- list(NA, NA)
  } else {
    result <- list("Non mobile", 0.008)
    if (Koc < 4000) result <- list("Slightly mobile", 0.02)
    if (Koc < 1000) result <- list("Slightly mobile", 0.5)
    if (Koc < 500) result <- list("Moderately mobile", 0.7)
    if (Koc < 75) result <- list("Mobile", 1.9)
    if (Koc < 15) result <- list("Very mobile", 1.9)
  }
  names(result) <- c("Mobility classification",
                     "Percentage drained per mm of drain water")
  return(result)
}
