% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twa.R
\name{one_box}
\alias{one_box}
\alias{one_box.numeric}
\alias{one_box.character}
\alias{one_box.mkinfit}
\title{Create a time series of decline data}
\usage{
one_box(x, ini, ..., t_end = 100, res = 0.01)

\method{one_box}{numeric}(x, ini = 1, ..., t_end = 100, res = 0.01)

\method{one_box}{character}(x, ini = 1, parms, ..., t_end = 100, res = 0.01)

\method{one_box}{mkinfit}(x, ini = "model", ..., t_end = 100, res = 0.01)
}
\arguments{
\item{x}{When numeric, this is the half-life to be used for an exponential
decline. When a character string specifying a parent decline model is given
e.g. \code{FOMC}, \code{parms} must contain the corresponding parameters.
If x is an \code{\link{mkinfit}} object, the decline is calculated from this
object.}

\item{ini}{The initial amount. If x is an \code{\link{mkinfit}} object, and
ini is 'model', the fitted initial concentrations are used. Otherwise, ini
must be numeric. If it has length one, it is used for the parent and
initial values of metabolites are zero, otherwise, it must give values for
all observed variables.}

\item{...}{Further arguments passed to methods}

\item{t_end}{End of the time series}

\item{res}{Resolution of the time series}

\item{parms}{A named numeric vector containing the model parameters}
}
\value{
An object of class \code{one_box}, inheriting from \code{\link{ts}}.
}
\description{
Create a time series of decline data
}
\examples{
# Only use a half-life
pred_0 <- one_box(10)
plot(pred_0)

# Use a fitted mkinfit model
require(mkin)
fit <- mkinfit("FOMC", FOCUS_2006_C, quiet = TRUE)
pred_1 <- one_box(fit)
plot(pred_1)

# Use a model with more than one observed variable
m_2 <- mkinmod(parent = mkinsub("SFO", "m1"), m1 = mkinsub("SFO"))
fit_2 <- mkinfit(m_2, FOCUS_2006_D, quiet = TRUE)
pred_2 <- one_box(fit_2, ini = "model")
plot(pred_2)
}
