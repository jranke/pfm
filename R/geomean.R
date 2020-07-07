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
