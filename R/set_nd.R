#' Set non-detects in residue series without replicates

#' Sets non-detects directly before or directly after detects to NA. Values between
#' lod and loq are set to their mean value if an loq is specified.
#' If 'time_zero' is set to TRUE, the residue series is assumed to start with time
#' zero, and non-detects at time zero are set to 'time_zero_nd_value'. For the
#' set_nd_focus variant, this is zero, otherwise this argument has NA as default
#' value.
#' If stopping after the first non-detection is requested, as in in the FOCUS
#' variant of the function, an loq has to be specified in order to decide
#' if any later detections are above the loq.

#' @param r A character vector of sequential residues without replicates, with
#' non-detects specified as 'nd' and unquantified values above the limit of
#' detection specified as 'nq', otherwise coercible to numeric
#' @param lod Limit of detection (numeric)
#' @param loq Limit of quantification(numeric). Must be specified if the FOCUS rule to
#' stop after the first non-detection is to be applied
#' @param time_zero Is the first value in the series a time zero value?
#' @param time_zero_nd_value Which value should we use for non-detects at time zero?
#' @param stop_after_first_nondetect Should we really stop after the first non-detection?
#' @references FOCUS (2014) p. 75, 76, 131, 132
#' @export
#' @examples
#' parent_1 <- c(.12, .09, .05, .03, "nd", "nd", "nd", "nd", "nd", "nd")
#' set_nd(parent_1, 0.02)
#' parent_2 <- c(.12, .09, .05, .03, "nd", "nd", .03, "nd", "nd", "nd")
#' set_nd(parent_2, 0.02)
#' set_nd_focus(parent_2, 0.02, loq = 0.05)
#' parent_3 <- c(.12, .09, .05, .03, "nd", "nd", .06, "nd", "nd", "nd")
#' set_nd(parent_3, 0.02)
#' set_nd_focus(parent_3, 0.02, loq = 0.05)
#' metabolite <- c("nd", "nd", "nd", 0.03, 0.06, 0.10, 0.11, 0.10, 0.09, 0.05, 0.03, "nd", "nd")
#' set_nd(metabolite, 0.02)
set_nd <- function(r, lod, loq = NA,
  time_zero = TRUE, time_zero_nd_value = NA, stop_after_first_nondetect = FALSE)
{

  if (stop_after_first_nondetect & is.na(loq)) {
    stop("You need to specify an loq to decide if the curve should be cut off after the first non-detect")
  }

  result <- r

  # Handle nq values
  if (!missing(loq)) {
    nq = 0.5 * (lod + loq)
    result[r == "nq"] <- nq
  } else {
    if (any(r == "nq")) stop("You need to specify lod and loq")
  }

  #  Handle nd values
  if (time_zero) {
    if (r[1] == "nd") {
      residues_present = FALSE
      result[1] <- time_zero_nd_value
    } else {
      residues_present = TRUE
    }
    start_i <- 2
  } else {
    residues_present <- if (r[1] == "nd") FALSE else TRUE
    start_i <- 1
  }

  for (i in start_i:length(r)) {

    # residues_in_next
    if (i < length(r)) {
      next_value <- r[i + 1]
      if (next_value == "nd") residues_in_next = FALSE
      else residues_in_next = TRUE
    } else {
      residues_in_next = FALSE
    }

    if (r[i] == "nd") {
      if (residues_present | residues_in_next) {
        result[i] <- 0.5 * lod
      } else {
        result[i] <- NA
      }

      if (stop_after_first_nondetect) {
        if (residues_present & !residues_in_next) {
          remaining <- (i + 1):length(r)
          if (!any(suppressWarnings(as.numeric(r[remaining])) > loq, na.rm = TRUE)) {
            result[remaining] <- NA
            return(as.numeric(result))
          }
        }

      }
      if (!residues_in_next) residues_present <- FALSE
      else residues_present <- TRUE
    }

  }
  return(as.numeric(result))
}

#' @describeIn set_nd Set non-detects in residues series according to FOCUS rules
#' @export
set_nd_focus <- function(r, lod, loq = NA, time_zero = TRUE) {
  result <- set_nd(r, lod, loq = loq, time_zero = time_zero,
    time_zero_nd_value = 0, stop_after_first_nondetect = TRUE)
  return(result)
}
