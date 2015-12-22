#' Deposition from spray drift expressed as percent of the applied dose as
#' published by the JKI
#' 
#' Deposition from spray drift expressed as percent of the applied dose as
#' published by the German Julius-Kühn Institute (JKI).
#' 
#' The data were extracted from the spreadsheet cited below using the R code
#' given in the example section. The spreadsheet is not included in the package
#' as its licence is not clear.
#' 
#' 
#' @name drift_data_JKI
#' @docType data
#' @format A list currently containing matrices with spray drift percentage
#' data for field crops (Ackerbau), and Pome/stone fruit, early and late
#' (Obstbau frueh, spaet).
#' @source JKI (2010) Spreadsheet 'Tabelle der Abdrifteckwerte.xls', retrieved
#' from
#' http://www.jki.bund.de/no_cache/de/startseite/institute/anwendungstechnik/abdrift-eckwerte.html
#' on 2015-06-11
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#'   # This is the code that was used to extract the data
#'   library(readxl)
#'   abdrift_path <- "inst/extdata/Tabelle der Abdrifteckwerte.xls"
#'   JKI_crops <- c("Ackerbau", "Obstbau frueh", "Obstbau spaet")
#'   names(JKI_crops) <- c("Field crops", "Pome/stone fruit, early", "Pome/stone fruit, late")
#'   drift_data_JKI <- list()
#' 
#'   for (n in 1:8) {
#'     drift_data_raw <- read_excel(abdrift_path, sheet = n + 1, skip = 2)
#'     drift_data <- as.matrix(drift_data_raw[1:9, 2:4]) 
#'     dimnames(drift_data) <- list(distance = as.integer(drift_data_raw[1:9, 1]),
#'                                             crop = JKI_crops)
#'     drift_data_JKI[[n]] <- drift_data
#'   }
#'   save(drift_data_JKI, file = "data/drift_data_JKI.RData")
#' }
#' 
#' # And this is the resulting data
#' drift_data_JKI
NULL
