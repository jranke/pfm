# Copyright (C) 2015,2018,2019  Johannes Ranke
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
