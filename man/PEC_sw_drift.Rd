% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PEC_sw_drift.R
\name{PEC_sw_drift}
\alias{PEC_sw_drift}
\title{Calculate predicted environmental concentrations in surface water due to drift}
\usage{
PEC_sw_drift(
  rate,
  applications = 1,
  water_depth = as_units("30 cm"),
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
  crop_group_JKI = c("Ackerbau", "Obstbau frueh", "Obstbau spaet", "Weinbau frueh",
    "Weinbau spaet", "Hopfenbau", "Flaechenkulturen > 900 l/ha", "Gleisanlagen"),
  crop_group_RF = c("arable", "hops", "vines, late", "vines, early", "fruit, late",
    "fruit, early", "aerial"),
  distances = c(1, 5, 10, 20),
  formula = c("Rautmann", "FOCUS"),
  water_width = as_units("100 cm"),
  side_angle = 90,
  rate_units = "g/ha",
  PEC_units = "µg/L"
)
}
\arguments{
\item{rate}{Application rate in units specified below, or with units defined via the
\code{units} package.}

\item{applications}{Number of applications for selection of drift percentile}

\item{water_depth}{Depth of the water body in cm}

\item{drift_percentages}{Percentage drift values for which to calculate PECsw.
Overrides 'drift_data' and 'distances' if not NULL.}

\item{drift_data}{Source of drift percentage data. If 'JKI', the \link{drift_data_JKI}
included in the package is used. If 'RF', the Rautmann drift data are calculated
either in the original form or integrated over the width of the water body, depending
on the 'formula' argument.}

\item{crop_group_JKI}{When using the 'JKI' drift data, one of the German names
as used in \link{drift_data_JKI}. Will only be used if drift_data is 'JKI'.}

\item{crop_group_RF}{One of the crop groups as used in \link{drift_parameters_focus}}

\item{distances}{The distances in m for which to get PEC values}

\item{formula}{By default, the original Rautmann formula is used. If you
specify "FOCUS", mean drift input over the width of the water body is
calculated as described in Chapter 5.4.5 of the FOCUS surface water guidance}

\item{water_width}{Width of the water body in cm}

\item{side_angle}{The angle of the side of the water relative to the bottom which
is assumed to be horizontal, in degrees. The SYNOPS model assumes 45 degrees here.}

\item{rate_units}{Defaults to g/ha. For backwards compatibility, only used
if the specified rate does not have \link[units:units]{units::units}].}

\item{PEC_units}{Requested units for the calculated PEC. Only µg/L currently supported}
}
\value{
The predicted concentration in surface water
}
\description{
This is a basic, vectorised form of a simple calculation of a contaminant
concentration in surface water based on complete, instantaneous mixing
with input via spray drift.
}
\details{
It is recommened to specify the arguments \code{rate}, \code{water_depth} and
\code{water_width} using \link[units:units]{units::units} from the \code{units} package.
}
\examples{
PEC_sw_drift(100)
# Alternatively, we can use the formula for a single application to
# "Ackerbau" from the paper
PEC_sw_drift(100, drift_data = "RF")

# This makes it possible to also use different distances
PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF")

# or consider aerial application
PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF",
  crop_group_RF = "aerial")

# Using custom drift percentages is also supported
PEC_sw_drift(100, drift_percentages = c(2.77, 0.95, 0.57, 0.48, 0.29, 0.15, 0.06, 0.03))

# The influence of assuming a 45° angle of the sides of the waterbody and the width of the
# waterbody can be illustrated
PEC_sw_drift(100)
PEC_sw_drift(100, drift_data = "RF")
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS")
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45)
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45, water_width = 200)
}
\seealso{
\link{drift_parameters_focus}, \link{drift_percentages_rautmann}
}
\author{
Johannes Ranke
}
