#' Groundwater ubiquity score based on Gustafson (1989)
#' 
#' The groundwater ubiquity score GUS is calculated according to
#' the following equation
#' \deqn{GUS = \log_10 DT50_{soil} (4 - \log_10 K_{oc}}{GUS = log10 DT50soil * (4 - log10 Koc)}
#' 
#' @references Gustafson, David I. (1989) Groundwater ubiquity score: a simple
#' method for assessing pesticide leachability. _Environmental
#' toxicology and chemistry_ *8*(4) 339–57.
#' @inheritParams endpoint
#' @param chent If a chent is given with appropriate information present in its
#'   chyaml field, this information is used, with defaults specified below.
#' @param DT50 Half-life of the chemical in soil. Should be a field
#'   half-life according to Gustafson (1989). However, leaching to the sub-soil
#'   can not completely be excluded in field dissipation experiments and Gustafson
#'   did not refer to any normalisation procedure, but says the field study should
#'   be conducted under use conditions.
#' @param Koc The sorption constant normalised to organic carbon. Gustafson
#'   does not mention the nonlinearity of the sorption constant commonly
#'   found and usually described by Freundlich sorption, therefore it is 
#'   unclear at which reference concentration the Koc should be observed
#'   (and if the reference concentration would be in soil or in porewater).
#' @param lab_field Should laboratory or field half-lives be used? This
#'   defaults to lab in this implementation, in order to avoid
#'   double-accounting for mobility. If comparability with the original GUS
#'   values given by Gustafson (1989) is desired, non-normalised first-order
#'   field half-lives obtained under actual use conditions should be used.
#' @param degradation_value Which of the available degradation values should 
#'   be used?
#' @param sorption_value Which of the available sorption values should be used?
#'   Defaults to Kfoc as this is what is generally available from the European
#'   pesticide peer review process. These values generally use a reference
#'   concentration of 1 mg/L in porewater, that means they would be expected to
#'   be Koc values at a concentration of 1 mg/L in the water phase.
#' @param degradation_aggregator Function for aggregating half-lives
#' @param sorption_aggregator Function for aggregation Koc values
#' @return A list with the DT50 and Koc used as well as the resulting score
#'   of class GUS_result
#' @author Johannes Ranke
#' @export
GUS <- function(...) UseMethod("GUS")

#' @rdname GUS
#' @export
GUS.numeric <- function(DT50, Koc) {
  score <- log10(DT50) * (4 - log10(Koc))
  res <- list(DT50 = DT50, Koc = Koc, score = score)
  class(res) <- "GUS_result"
  return(res)
}

#' @rdname GUS
#' @export
GUS.chent <- function(chent, lab_field = "laboratory",
                      aerobic = TRUE,
                      degradation_value = "DT50ref",
                      sorption_value = "Kfoc", 
                      degradation_aggregator = geomean,
                      sorption_aggregator = geomean,
                      digits = 1)
{
  DT50 = soil_DT50(chent, lab_field = lab_field, redox = aerobic, 
                   value = degradation_value, 
                   aggregator = degradation_aggregator, signif = 5)
  Koc = soil_Kfoc(chent, value = sorption_value, 
                   aggregator = sorption_aggregator, signif = 5)
  GUS.numeric(DT50, Koc)
}

#' @export
print.GUS_result = function(x, ..., digits = 1) {
  cat("GUS: ", round(x$score, digits = 1), "\n")
  cat("calculated from DT50 ", x$DT50, " and Koc ", x$Koc, "\n")
}
