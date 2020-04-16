# Copyright (C) 2015,2020  Johannes Ranke
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

#' Calculate the geometric mean
#'
#' Based on some posts in a thread on Stackoverflow
#' \url{http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}
#' This function returns NA if NA values are present and na.rm = FALSE
#' (default). If negative values are present, it gives an error message.
#' If at least one element of the vector is 0, it returns 0.
#'
#' @param x Vector of numbers
#' @param na.rm Should NA values be omitted?
#' @return The geometric mean
#' @export
#' @author Johannes Ranke
#' @examples
#' geomean(c(1, 3, 9))
#' geomean(c(1, 3, NA, 9))
#' \dontrun{geomean(c(1, -3, 9)) # returns an error}
geomean = function(x, na.rm = FALSE) {
  if (any(is.na(x)) & na.rm == FALSE) return(NA)
  if (any(x < 0, na.rm = na.rm)) stop("Only defined for positive numbers")
  exp(mean(log(x), na.rm = na.rm))
}
