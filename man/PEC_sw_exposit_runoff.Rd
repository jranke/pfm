% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PEC_sw_exposit.R
\name{PEC_sw_exposit_runoff}
\alias{PEC_sw_exposit_runoff}
\title{Calculate PEC surface water due to runoff and erosion as in Exposit 3}
\source{
Excel 3.02 spreadsheet available from
\url{https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html}
}
\usage{
PEC_sw_exposit_runoff(
  rate,
  interception = 0,
  Koc,
  DT50 = set_units(Inf, "d"),
  t_runoff = set_units(3, "days"),
  exposit_reduction_version = c("3.02", "3.01a", "3.01a2", "2.0"),
  V_ditch = set_units(30, "m3"),
  V_event = set_units(100, "m3"),
  dilution = 2
)
}
\arguments{
\item{rate}{The application rate in g/ha}

\item{interception}{The fraction intercepted by the crop}

\item{Koc}{The sorption coefficient to soil organic carbon}

\item{DT50}{The soil half-life in days}

\item{t_runoff}{The time between application and the runoff event, where degradation occurs, in days}

\item{exposit_reduction_version}{The version of the reduction factors to be used. "3.02" is the current
version used in Germany, "3.01a" is the version with additional percentages for 3 m and 6 m buffer
zones used in Switzerland. "3.01a2" is a version introduced for consistency with previous calculations
performed for a 3 m buffer zone in Switzerland, with the same reduction being applied to the dissolved
and the bound fraction.}

\item{V_ditch}{The volume of the ditch is assumed to be 1 m * 100 m * 30 cm = 30 m3}

\item{V_event}{The unreduced runoff volume, equivalent to 10 mm precipitation on 1 ha}

\item{dilution}{The dilution factor}
}
\value{
A list containing the following components
\describe{
\item{perc_runoff}{The runoff percentages for dissolved and bound substance}
\item{runoff}{A matrix containing dissolved and bound input for the different distances}
\item{PEC_sw_runoff}{A matrix containing PEC values for dissolved and bound substance
for the different distances. If the rate was given in g/ha, the PECsw are in microg/L.}
}
}
\description{
This is a reimplementation of the calculation described in the Exposit 3.02 spreadsheet file,
in the worksheet "Konzept Runoff".
}
\details{
It is recommened to specify the arguments \code{rate}, \code{Koc}, \code{DT50}, \code{t_runoff}, \code{V_ditch} and \code{V_event}
using \link[units:units]{units::units} from the \code{units} package.
}
\examples{
  PEC_sw_exposit_runoff(500, Koc = 150)
  PEC_sw_exposit_runoff(600, Koc = 10000, DT50 = 195, exposit = "3.01a")
}
\seealso{
\code{\link{perc_runoff_exposit}} for runoff loss percentages and \code{\link{perc_runoff_reduction_exposit}} for runoff reduction percentages used
}
