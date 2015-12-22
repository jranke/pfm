# Copyright (C) 2015  Johannes Ranke
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

#' Calculate a time course of relative concentrations based on an mkinmod model
#'
#' @import mkin
#' @param model The degradation model to be used. Either a parent only model like
#'   'SFO' or 'FOMC', or an mkinmod object
#' @param DT50 The half-life. This is only used when simple exponential decline
#'   is calculated (SFO model).
#' @param parms The parameters used for the degradation model
#' @param years For how many years should the degradation be predicted?
#' @param step_days What step size in days should the output have?
#' @param times The output times
#' @export
#' @author Johannes Ranke
#' @examples
#' head(pfm_degradation("SFO", DT50 = 10))
pfm_degradation <- function(model = "SFO", DT50 = 1000, parms = c(k_parent_sink = log(2)/DT50),
                            years = 1, step_days = 1,
                            times = seq(0, years * 365, by = step_days))
{
  if (model %in% c("SFO", "FOMC", "DFOP", "HS", "IORE")) {
    model <- mkinmod(parent = list(type = model))
  }
  initial_state = c(1, rep(0, length(model$diffs) - 1))
  names(initial_state) <- names(model$diffs)
  time_course <- mkinpredict(model, odeparms = parms, 
                             odeini = initial_state,
                             outtimes = times,
                             solution_type = ifelse(length(model$spec) == 1,
                                                    "analytical", "deSolve"))
  invisible(time_course)
}
