#' Actual and maximum moving window time average concentrations for FOMC kinetics
#'
#' @param alpha Parameter of the FOMC model
#' @param beta Parameter of the FOMC model
#' @param times The output times, and window sizes for time weighted average concentrations
#' @export
#' @author Johannes Ranke
#' @source FOCUS (2014) Generic Guidance for Estimating Persistence and Degradation
#'   Kinetics from Environmental Fate Studies on Pesticides in EU Registration, Version 1.1,
#'   18 December 2014, p. 251
#' @examples
#' FOMC_actual_twa(alpha = 1.0001, beta = 10)
FOMC_actual_twa <- function(alpha = 1.0001, beta = 10, times = c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100))
{
  result <- data.frame(actual = 1 / (times/beta + 1)^alpha,
                       twa = (beta / (times * (1 - alpha))) * (((times / beta) + 1)^(1 - alpha) - 1),
                       row.names = times)
  return(result)
}
