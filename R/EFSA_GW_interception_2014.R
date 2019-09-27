#' Subset of EFSA crop interception default values for groundwater modelling
#'
#' @name EFSA_GW_interception_2014
#' @docType data
#' @source European Food Safety Authority (2014) EFSA Guidance Document for
#' evaluating laboratory and field dissipation studies to obtain DegT50 values
#' of active substances of plant protection products and transformation
#' products of these active substances in soil. \emph{EFSA Journal}
#' \bold{12}(5):3662, 37 pp., doi:10.2903/j.efsa.2014.3662
#' @format A matrix containing interception values, currently only for some selected crops
#' @examples
#' \dontrun{
#'   # This is the code that was used to define the data
#'   bbch <- paste0(0:9, "x")
#'   crops <- c(
#'     "Beans (field + vegetable)",
#'     "Peas",
#'     "Summer oilseed rape", "Winter oilseed rape",
#'     "Tomatoes",
#'     "Spring cereals", "Winter cereals")
#'   EFSA_GW_interception_2014 <- matrix(NA, length(crops), length(bbch),
#'     dimnames = list(Crop = crops, BBCH = bbch))
#'   EFSA_GW_interception_2014["Beans (field + vegetable)", ] <-
#'     c(0, 0.25, rep(0.4, 2), rep(0.7, 5), 0.8)
#'   EFSA_GW_interception_2014["Peas", ] <-
#'     c(0, 0.35, rep(0.55, 2), rep(0.85, 5), 0.85)
#'   EFSA_GW_interception_2014["Summer oilseed rape", ] <-
#'     c(0, 0.4, rep(0.8, 2), rep(0.8, 5), 0.9)
#'   EFSA_GW_interception_2014["Winter oilseed rape", ] <-
#'     c(0, 0.4, rep(0.8, 2), rep(0.8, 5), 0.9)
#'   EFSA_GW_interception_2014["Tomatoes", ] <-
#'     c(0, 0.5, rep(0.7, 2), rep(0.8, 5), 0.5)
#'   EFSA_GW_interception_2014["Spring cereals", ] <-
#'     c(0, 0, 0.2, 0.8, rep(0.9, 3), rep(0.8, 2), 0.8)
#'   EFSA_GW_interception_2014["Winter cereals", ] <-
#'     c(0, 0, 0.2, 0.8, rep(0.9, 3), rep(0.8, 2), 0.8)
#'   save(EFSA_GW_interception_2014,
#'     file = "../data/EFSA_GW_interception_2014.RData")
#' }
#' EFSA_GW_interception_2014
NULL
