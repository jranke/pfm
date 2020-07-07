#' Actual and maximum moving window time average concentrations for SFO kinetics
#'
#' @param DT50 The half-life.
#' @param times The output times, and window sizes for time weighted average concentrations
#' @export
#' @author Johannes Ranke
#' @source FOCUS (2014) Generic Guidance for Estimating Persistence and Degradation
#'   Kinetics from Environmental Fate Studies on Pesticides in EU Registration, Version 1.1, 
#'   18 December 2014, p. 251
#' @examples
#' SFO_actual_twa(10)
SFO_actual_twa <- function(DT50 = 1000, times = c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100))
{
  k = log(2)/DT50
  result <- data.frame(actual = 1 * exp(-k * times),
                       twa = (1 - exp(-k * times))/(k * times),
                       row.names = times)
  return(result)
}
