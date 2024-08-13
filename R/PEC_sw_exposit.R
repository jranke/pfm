#' Runoff loss percentages as used in Exposit 3
#'
#' A table of the loss percentages used in Exposit 3 for the twelve different Koc classes
#'
#' @name perc_runoff_exposit
#' @format A data frame with percentage values for the dissolved fraction and the fraction
#'   bound to eroding particles, with Koc classes used as row names
#'   \describe{
#'     \item{Koc_lower_bound}{The lower bound of the Koc class}
#'     \item{dissolved}{The percentage of the applied substance transferred to an
#'       adjacent water body in the dissolved phase}
#'     \item{bound}{The percentage of the applied substance transferred to an
#'       adjacent water body bound to eroding particles}
#'   }
#' @source Excel 3.02 spreadsheet available from
#'   \url{https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html}
#' @docType data
#' @examples
#' print(perc_runoff_exposit)
"perc_runoff_exposit"

#' Runoff reduction percentages as used in Exposit
#'
#' A table of the runoff reduction percentages used in Exposit 3 for different vegetated buffer widths
#'
#' @name perc_runoff_reduction_exposit
#' @format A named list of data frames with reduction percentage values for the
#' dissolved fraction and the fraction bound to eroding particles, with
#' vegetated buffer widths as row names. The names of the list items are the Exposit versions
#' from which the values were taken.
#'   \describe{
#'     \item{dissolved}{The reduction percentage for the dissolved phase}
#'     \item{bound}{The reduction percentage for the particulate phase}
#'   }
#' @source Excel 3.02 spreadsheet available from
#'   \url{https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html}
#'
#'   Agroscope version 3.01a with additional runoff factors for 3 m and 6 m buffer zones received from Muris Korkaric (not published).
#'   The variant 3.01a2 was introduced for consistency with previous calculations performed by Agroscope for a 3 m buffer zone.
#' @docType data
#' @examples
#' print(perc_runoff_reduction_exposit)
"perc_runoff_reduction_exposit"

#' Calculate PEC surface water due to runoff and erosion as in Exposit 3
#'
#' This is a reimplementation of the calculation described in the Exposit 3.02 spreadsheet file,
#' in the worksheet "Konzept Runoff".
#'
#' It is recommened to specify the arguments `rate`, `Koc`, `DT50`, `t_runoff`, `V_ditch` and `V_event`
#' using [units::units] from the `units` package.
#'
#' @importFrom units as_units set_units drop_units
#' @importFrom dplyr across mutate
#' @param rate The application rate in g/ha
#' @param interception The fraction intercepted by the crop
#' @param Koc The sorption coefficient to soil organic carbon
#' @param DT50 The soil half-life in days
#' @param t_runoff The time between application and the runoff event, where degradation occurs, in days
#' @param exposit_reduction_version The version of the reduction factors to be used. "3.02" is the current
#'   version used in Germany, "3.01a" is the version with additional percentages for 3 m and 6 m buffer
#'   zones used in Switzerland. "3.01a2" is a version introduced for consistency with previous calculations
#'   performed for a 3 m buffer zone in Switzerland, with the same reduction being applied to the dissolved
#'   and the bound fraction.
#' @param V_ditch The volume of the ditch is assumed to be 1 m * 100 m * 30 cm = 30 m3
#' @param V_event The unreduced runoff volume, equivalent to 10 mm precipitation on 1 ha
#' @param dilution The dilution factor
#' @return A list containing the following components
#'   \describe{
#'     \item{perc_runoff}{The runoff percentages for dissolved and bound substance}
#'     \item{runoff}{A matrix containing dissolved and bound input for the different distances}
#'     \item{PEC_sw_runoff}{A matrix containing PEC values for dissolved and bound substance
#'       for the different distances. If the rate was given in g/ha, the PECsw are in microg/L.}
#'   }
#' @export
#' @source Excel 3.02 spreadsheet available from
#'   \url{https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html}
#' @seealso \code{\link{perc_runoff_exposit}} for runoff loss percentages and \code{\link{perc_runoff_reduction_exposit}} for runoff reduction percentages used
#' @examples
#'   PEC_sw_exposit_runoff(500, Koc = 150)
#'   PEC_sw_exposit_runoff(600, Koc = 10000, DT50 = 195, exposit = "3.01a")
PEC_sw_exposit_runoff <- function(rate, interception = 0, Koc, DT50 = set_units(Inf, "d"), 
  t_runoff = set_units(3, "days"),
  exposit_reduction_version = c("3.02", "3.01a", "3.01a2", "2.0"),
  V_ditch = set_units(30, "m3"), V_event = set_units(100, "m3"), dilution = 2)
{
  # Set default units if not specified
  if (!inherits(rate, "units")) rate <- set_units(rate, "g/ha")
  if (!inherits(Koc, "units")) Koc <- set_units(Koc, "L/kg")
  if (!inherits(DT50, "units")) DT50 <- set_units(DT50, "d")
  if (!inherits(t_runoff, "units")) t_runoff <- set_units(t_runoff, "d")
  if (!inherits(V_ditch, "units")) V_ditch <- set_units(V_ditch, "m3")
  if (!inherits(V_event, "units")) V_event <- set_units(V_event, "m3")

  k_deg <- log(2)/DT50
  
  # The input is calculated for an area of 1 ha
  input <- rate * as_units(1, "ha") * (1 - interception) * exp(as.numeric(-k_deg * t_runoff))
  input_units <- units(input)
  input_numeric <- drop_units(input)

  if (length(Koc) > 1) stop("Only one compound at a time supported")

  exposit_reduction_version <- match.arg(exposit_reduction_version)
  reduction_runoff <- pfm::perc_runoff_reduction_exposit[[exposit_reduction_version]] / 100
        
  transfer_runoff <- 1 - reduction_runoff

  V_runoff <- V_event * (1 - reduction_runoff[["dissolved"]])
  V_flowing_ditch_runoff <- dilution * (V_ditch + V_runoff)
  
  f_runoff_exposit <- function(Koc) {
    Koc_breaks <- c(pfm::perc_runoff_exposit$Koc_lower_bound, set_units(Inf, "L/kg"))
    Koc_classes <- as.character(cut(Koc, Koc_breaks, labels = rownames(pfm::perc_runoff_exposit)))
    perc_runoff <- pfm::perc_runoff_exposit[Koc_classes, c("dissolved", "bound")]
    if (identical(Koc, 0)) perc_runoff <- c(dissolved = 0, bound = 0)
    return(unlist(perc_runoff) / 100)
  }
  f_runoff <- f_runoff_exposit(Koc)

  runoff_dissolved <- input_numeric * f_runoff["dissolved"] * transfer_runoff[, "dissolved"]
  runoff_bound <- input_numeric * f_runoff["bound"] * transfer_runoff[, "bound"]
  runoff_input <- cbind(dissolved = runoff_dissolved, bound = runoff_bound, 
    total = runoff_dissolved + runoff_bound)
  rownames(runoff_input) <- rownames(reduction_runoff)
  units(runoff_input) <- input_units

  dn <- dimnames(runoff_input)
  PEC_sw_runoff <- set_units(runoff_input / V_flowing_ditch_runoff, "\u00B5g/L")
  dimnames(PEC_sw_runoff) <- dn

  result <- list(
    perc_runoff = 100 * f_runoff,
    runoff = as.data.frame(runoff_input),
    PEC_sw_runoff = as.data.frame(PEC_sw_runoff))
  return(result)
}

#' Calculate PEC surface water due to drainage as in Exposit 3
#'
#' This is a reimplementation of the calculation described in the Exposit 3.02 spreadsheet file,
#' in the worksheet "Konzept Drainage". Although there are four groups of
#' compounds ("Gefährdungsgruppen"), only one distinction is made in the
#' calculations, between compounds with low mobility (group 1) and compounds
#' with modest to high mobility (groups 2, 3 and 4). In this implementation,
#' the group is derived only from the Koc, if not given explicitly. For
#' details, see the discussion of the function arguments below.
#'
#' @param rate The application rate in g/ha
#' @param interception The fraction intercepted by the crop
#' @param Koc The sorption coefficient to soil organic carbon used to determine the mobility. A trigger
#'   value of 550 L/kg is used in order to decide if Koc >> 500.
#' @param mobility Overrides what is determined from the Koc.
#' @param DT50 The soil half-life in days
#' @param t_drainage The time between application and the drainage event, where degradation occurs, in days
#' @param V_ditch The volume of the ditch is assumed to be 1 m * 100 m * 30 cm = 30 m3
#' @param V_drainage The drainage volume, equivalent to 1 mm precipitation on 1 ha for spring/summer or 10 mm for
#'   autumn/winter/early spring.
#' @param dilution The dilution factor
#' @return A list containing the following components
#'   \describe{
#'     \item{perc_runoff}{The runoff percentages for dissolved and bound substance}
#'     \item{runoff}{A matrix containing dissolved and bound input for the different distances}
#'     \item{PEC_sw_runoff}{A matrix containing PEC values for dissolved and bound substance
#'       for the different distances. If the rate was given in g/ha, the PECsw are in microg/L.}
#'   }
#' @export
#' @source Excel 3.02 spreadsheet available from
#'   \url{https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html}
#' @seealso \code{\link{perc_runoff_exposit}} for runoff loss percentages and \code{\link{perc_runoff_reduction_exposit}} for runoff reduction percentages used
#' @examples
#'   PEC_sw_exposit_drainage(500, Koc = 150)
PEC_sw_exposit_drainage <- function(rate, interception = 0, Koc = NA, mobility = c(NA, "low", "high"), DT50 = Inf, t_drainage = 3,
  V_ditch = 30, V_drainage = c(spring = 10, autumn = 100), dilution = 2)
{
  # Rückstand zum Zeitpunkt des Niederschlagsereignisses (residue at the time of the drainage event)
  k_deg <- log(2)/DT50
  residue <- rate * (1 - interception) * 1 * exp(-k_deg * t_drainage) # assumes 1 ha treated area

  mobility <- match.arg(mobility)
  if (is.na(mobility)) {
    if (is.na(Koc)) stop("Koc is needed if the mobility is not specified")
    else {
      if (Koc > 550) mobility = "low"
      else mobility = "high"
    }
  }

  V_ditch_drainage <- V_ditch + V_drainage
  V_flowing_ditch_drainage <- dilution * V_ditch_drainage

  # Gesamtaustrag (total fraction of the residue drained)
  if (mobility == "low") {
    f_drainage_total <- c(spring = 0.01 * 1e-2,
                          autumn = 0.05 * 1e-2)
  } else {
    f_drainage_total <- c(spring = 0.2 * 1e-2,
                          autumn = 1.0 * 1e-2)
  }

  f_peak = c(spring = 0.125, autumn = 0.25) # Stoßbelastung (fraction drained at event)

  PEC_sw_drainage <- 1000 * residue * f_drainage_total * f_peak / V_flowing_ditch_drainage

  result <- list(
    perc_drainage_total = 100 * f_drainage_total,
    perc_peak = 100 * f_peak,
    PEC_sw_drainage = PEC_sw_drainage)
  return(result)
}
