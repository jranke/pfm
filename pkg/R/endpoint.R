#' Retrieve endpoint information from the chyaml field of a chent object
#'
#' R6 class objects of class \code{\link{chent}} represent chemical entities
#' and can hold a list of information loaded from a chemical yaml file in their
#' chyaml field. Such information is extracted and optionally aggregated by
#' this function.
#' 
#' The functions \code{soil_*} are functions to extract soil specific endpoints.
#' For the Freundlich exponent, the capital letter \code{N} is used in order to
#' facilitate dealing with such data in R. In pesticide fate modelling, this
#' exponent is often called 1/n.
#'
#' @import chents
#' @export
#' @param chent The \code{\link{chent}} object to get the information from
#' @param medium The medium for which information is sought
#' @param type The information type
#' @param lab_field If not NA, do we want laboratory or field endpoints
#' @param redox If not NA, are we looking for aerobic or anaerobic data
#' @param value The name of the value we want. The list given in the 
#'   usage section is not exclusive
#' @param aggregator The aggregator function. Can be mean, 
#'   \code{\link{geomean}}, or identity, for example.
#' @param raw Should the number(s) be returned as stored in the chent
#'   object (could be a character value) to retain original information
#'   about precision?
#' @param signif How many significant digits do we want
#' @return The result from applying the aggregator function to
#'   the values converted to a numeric vector, rounded to the
#'   given number of significant digits, or, if raw = TRUE,
#'   the values as a character value, retaining any implicit
#'   information on precision that may be present.
#' 
endpoint <- function(chent, 
                     medium = "soil",
                     type = c("degradation", "sorption"), 
                     lab_field = c(NA, "laboratory", "field"),
                     redox = c(NA, "aerobic", "anaerobic"),
                     value = c("DT50ref", "Kfoc", "N"),
                     aggregator = geomean,
                     raw = FALSE,
                     signif = 3)
{
  if (!is(chent, "chent")) {
    stop("Please supply a chent object as created using the package 'chents' available from jrwb.de")
  }
  ep_list <- chent$chyaml[[medium]][[type]] 
  if (!is.na(lab_field[1])) {
    ep_list <- ep_list[[lab_field]]
  }
  if (!is.na(redox[1])) {
    ep_list <- ep_list[[redox]]
  }
  values <- ep_list$data[[value]]
  if (raw) return(values)
  else return(signif(aggregator(as.numeric(values)), signif))
}

#' @inheritParams endpoint
#' @rdname endpoint
#' @export
soil_DT50 <- function(chent, aggregator = geomean, signif = 3, 
                      lab_field = "laboratory", value = "DT50ref",
                      redox = "aerobic", raw = FALSE) {
  ep <- endpoint(chent, medium = "soil", type = "degradation", 
                 lab_field = "laboratory", redox = redox,
                 signif = signif,
                 value = value, aggregator = aggregator, raw = raw)
  return(ep)
}

#' @inheritParams endpoint
#' @rdname endpoint
#' @export
soil_Kfoc <- function(chent, aggregator = geomean, signif = 3, 
                      value = "Kfoc", raw = FALSE) {
  ep <- endpoint(chent, medium = "soil", type = "sorption", 
                 signif = signif,
                 value = value, aggregator = aggregator, raw = raw)
  return(ep)
}

#' @inheritParams endpoint
#' @rdname endpoint
#' @export
soil_N <- function(chent, aggregator = mean, signif = 3, raw = FALSE) {
  ep <- endpoint(chent, medium = "soil", type = "sorption", 
                 signif = signif,
                 value = "N", aggregator = aggregator, raw = raw)
  return(ep)
}

#' @inheritParams endpoint
#' @rdname endpoint
#' @param values The values to be returned
#' @param aggregators A named vector of aggregator functions to be used
#' @export
soil_sorption <- function(chent, values = c("Kfoc", "N"), 
                          aggregators = c(Kfoc = geomean, Koc = geomean, N = mean), 
                          signif = c(Kfoc = 3, N = 3),
                          raw = FALSE) {
  res <- sapply(values, 
                function(x) {
                  endpoint(chent, medium = "soil", type = "sorption",
                           signif = signif[[x]],
                           value = x, aggregator = aggregators[[x]], raw = raw)
                }
  )
  return(res)
}
