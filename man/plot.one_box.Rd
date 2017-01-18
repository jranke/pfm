% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twa.R
\name{plot.one_box}
\alias{plot.one_box}
\title{Plot time series of decline data}
\usage{
\method{plot}{one_box}(x, xlim = range(time(x)), ylim = c(0, max(x)),
  xlab = "Time", ylab = "Fraction of initial", max_twa = NULL,
  max_twa_var = dimnames(x)[[2]][1], ...)
}
\arguments{
\item{x}{The object of type \code{\link{one_box}} to be plotted}

\item{xlim}{Limits for the x axis}

\item{ylim}{Limits for the y axis}

\item{xlab}{Label for the x axis}

\item{ylab}{Label for the y axis}

\item{max_twa}{If a numeric value is given, the maximum time weighted
average concentration(s) is/are shown in the graph.}

\item{max_twa_var}{Variable for which the maximum time weighted average should
be shown if max_twa is not NULL.}

\item{...}{Further arguments passed to methods}
}
\description{
Plot time series of decline data
}
\examples{
plot(sawtooth(one_box(10), 3, 7), max_twa = 21)
}