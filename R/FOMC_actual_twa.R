# Copyright (C) 2018  Johannes Ranke
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
