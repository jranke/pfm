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
#'   \url{https://www.bvl.bund.de/EN/04_PlantProtectionProducts/03_Applicants/04_AuthorisationProcedure/08_Environment/ppp_environment_node.html}
#' @export perc_runoff_exposit
#' @examples
#' print(perc_runoff_exposit)
{Koc_breaks <- c(0, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, Inf)
tmp <- paste(Koc_breaks[1:11], Koc_breaks[2:12], sep = "-")
Koc_classes <- c(tmp[1], paste0(">", tmp[2:11]), ">50000")}
perc_runoff_exposit <- data.frame(
  Koc_lower_bound = Koc_breaks[1:12],
  dissolved = c(0.11, 0.151, 0.197, 0.248, 0.224, 0.184, 0.133, 0.084, 0.037, 0.031, 0.014, 0.001),
  bound = c(0, 0, 0, 0.001, 0.004, 0.020, 0.042, 0.091, 0.159, 0.192, 0.291, 0.451))
rownames(perc_runoff_exposit) <- Koc_classes

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
#'   \url{https://www.bvl.bund.de/EN/04_PlantProtectionProducts/03_Applicants/04_AuthorisationProcedure/08_Environment/ppp_environment_node.html}
#'   
#'   Agroscope version 3.01a with additional runoff factors for 3 m and 6 m buffer zones received from Muris Korkaric (not published)
#' @export
#' @examples
#' print(perc_runoff_reduction_exposit)
perc_runoff_reduction_exposit <- list(
   "3.02" = data.frame(
    dissolved = c(0, 40, 60, 80),
    bound = c(0, 40, 85, 95),
    row.names = c("No buffer", paste(c(5, 10, 20), "m"))),
   "3.01a" = data.frame(
    dissolved = c(0, 25, 40, 45, 60, 80),
    bound = c(0, 7.1, 13.3, 15.9, 25.7, 48),
    #bound = c(0, 30, 40, 55, 85, 95),
    row.names = c("No buffer", paste(c(3, 5, 6, 10, 20), "m"))),
  "2.0" = data.frame(
    dissolved = c(0, 97.5),
    bound = c(0, 97.5),
    row.names = c("No buffer", "20 m"))
)

#' Calculate PEC surface water due to runoff and erosion as in Exposit 3
#'
#' This is a reimplementation of the calculation described in the Exposit 3.02 spreadsheet file,
#' in the worksheet "Konzept Runoff".
#'
#' @param rate The application rate in g/ha
#' @param interception The fraction intercepted by the crop
#' @param Koc The sorption coefficient to soil organic carbon
#' @param DT50 The soil half-life in days
#' @param t_runoff The time between application and the runoff event, where degradation occurs, in days
#' @param exposit_reduction_version The version of the reduction factors to be used. "3.02" is the current
#'   version used in Germany, "3.01a" is the version with additional percentages for 3 m and 6 m buffer
#'   zones used in Switzerland.
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
#'   \url{https://www.bvl.bund.de/DE/04_Pflanzenschutzmittel/03_Antragsteller/04_Zulassungsverfahren/07_Naturhaushalt/psm_naturhaush_node.html#doc1400590bodyText3}
#' @seealso \code{\link{perc_runoff_exposit}} for runoff loss percentages and \code{\link{perc_runoff_reduction_exposit}} for runoff reduction percentages used
#' @examples
#'   PEC_sw_exposit_runoff(500, Koc = 150)
#'   PEC_sw_exposit_runoff(600, Koc = 10000, DT50 = 195, exposit = "3.01a")
PEC_sw_exposit_runoff <- function(rate, interception = 0, Koc, DT50 = Inf, t_runoff = 3,
  exposit_reduction_version = c("3.02", "3.01a", "2.0"),
  V_ditch = 30, V_event = 100, dilution = 2)
{
  k_deg <- log(2)/DT50
  input <- rate * (1 - interception) * 1 * exp(-k_deg * t_runoff) # assumes 1 ha treated area

  if (length(Koc) > 1) stop("Only one compound at a time supported")

  exposit_reduction_version <- match.arg(exposit_reduction_version)
  red_water <- perc_runoff_reduction_exposit[[exposit_reduction_version]]["dissolved"] / 100
  red_bound <- perc_runoff_reduction_exposit[[exposit_reduction_version]]["bound"] / 100
  reduction_runoff <- perc_runoff_reduction_exposit[[exposit_reduction_version]] / 100
  transfer_runoff <- 1 - reduction_runoff

  V_runoff <- V_event * (1 - reduction_runoff[["dissolved"]]) # m3
  V_flowing_ditch_runoff <- dilution * (V_ditch + V_runoff)

  f_runoff_exposit <- function(Koc) {
    Koc_breaks <- c(perc_runoff_exposit$Koc_lower_bound, Inf)
    Koc_classes <- as.character(cut(Koc, Koc_breaks, labels = rownames(perc_runoff_exposit)))
    perc_runoff <- perc_runoff_exposit[Koc_classes, c("dissolved", "bound")]
    return(unlist(perc_runoff) / 100)
  }
  f_runoff <- f_runoff_exposit(Koc)
  runoff_dissolved <- input * f_runoff["dissolved"] * transfer_runoff["dissolved"]
  runoff_bound <- input * f_runoff["bound"] * transfer_runoff["bound"]
  runoff_input <- cbind(runoff_dissolved, runoff_bound)
  runoff_input$total <- runoff_input$dissolved + runoff_input$bound

  PEC_sw_runoff <- 1000 * runoff_input / V_flowing_ditch_runoff

  result <- list(
    perc_runoff = 100 * f_runoff,
    runoff = runoff_input,
    PEC_sw_runoff = PEC_sw_runoff)
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
#'   \url{https://www.bvl.bund.de/DE/04_Pflanzenschutzmittel/03_Antragsteller/04_Zulassungsverfahren/07_Naturhaushalt/psm_naturhaush_node.html#doc1400590bodyText3}
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
