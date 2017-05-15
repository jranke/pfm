#' Step 1/2 scenario data as distributed with the FOCUS Step 1/2 calculator
#'
#' The data were extracted from the scenario.txt file using the R code shown below.
#' The text file is not included in the package as its licence is not clear.
#'
#' @name FOCUS_Step_12_scenarios
#' @docType data
#' @format A list containing the scenario names in a character vector called 'names',
#'   the drift percentiles in a matrix called 'drift', interception percentages in
#'   a matrix called 'interception' and the runoff/drainage percentages for Step 2
#'   calculations in a matrix called 'rd'.
#' @keywords datasets
#' @examples
#'
#' \dontrun{
#'   # This is the code that was used to extract the data
#'   scenario_path <- "inst/extdata/FOCUS_Step_12_scenarios.txt"
#'   scenarios <- readLines(scenario_path)[9:38]
#'   FOCUS_Step_12_scenarios <- list()
#'   sce <- read.table(text = scenarios, sep = "\t", header = TRUE, check.names = FALSE,
#'     stringsAsFactors = FALSE)
#'   FOCUS_Step_12_scenarios$names = sce$Crop
#'   rownames(sce) <- sce$Crop
#'   FOCUS_Step_12_scenarios$drift = sce[, 3:11]
#'   FOCUS_Step_12_scenarios$interception = sce[, 12:15]
#'   sce_2 <- readLines(scenario_path)[41:46]
#'   rd <- read.table(text = sce_2, sep = "\t")[1:2]
#'   rd_mat <- matrix(rd$V2, nrow = 3, byrow = FALSE)
#'   dimnames(rd_mat) = list(Time = c("Oct-Feb", "Mar-May", "Jun-Sep"),
#'                           Region = c("North", "South"))
#'   FOCUS_Step_12_scenarios$rd = rd_mat
#'   save(FOCUS_Step_12_scenarios, file = "data/FOCUS_Step_12_scenarios.RData")
#' }
#'
#' # And this is the resulting data
#' FOCUS_Step_12_scenarios
NULL
