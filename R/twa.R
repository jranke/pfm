# Copyright (C) 2016,2017  Johannes Ranke

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

#' Create a time series of decline data
#'
#' @param x When numeric, this is the half-life to be used for an exponential
#'   decline. When a character string specifying a parent decline model is given
#'   e.g. \code{FOMC}, \code{parms} must contain the corresponding paramters.
#'   If x is an \code{\link{mkinfit}} object, the decline is calculated from this
#'   object.  
#' @param ini The initial amount. If x is an \code{\link{mkinfit}} object, and
#'   ini is 'model', the fitted initial concentrations are used. Otherwise, ini
#'   must be numeric. If it has length one, it is used for the parent and
#'   initial values of metabolites are zero, otherwise, it must give values for
#'   all observed variables.
#' @param t_end End of the time series
#' @param res Resolution of the time series
#' @param ... Further arguments passed to methods
#' @return An object of class \code{one_box}, inheriting from \code{\link{ts}}.
#' @importFrom stats filter frequency time ts
#' @export
#' @examples
#' # Only use a half-life
#' pred_0 <- one_box(10)
#' plot(pred_0)
#'
#' # Use a fitted mkinfit model
#' require(mkin)
#' fit <- mkinfit("FOMC", FOCUS_2006_C, quiet = TRUE)
#' pred_1 <- one_box(fit)
#' plot(pred_1)
#'
#' # Use a model with more than one observed variable
#' m_2 <- mkinmod(parent = mkinsub("SFO", "m1"), m1 = mkinsub("SFO"))
#' fit_2 <- mkinfit(m_2, FOCUS_2006_D, quiet = TRUE)
#' pred_2 <- one_box(fit_2, ini = "model")
#' plot(pred_2)
one_box <- function(x, ini, ...,
  t_end = 100, res = 0.01)
{
  UseMethod("one_box")
}

#' @rdname one_box
#' @export
one_box.numeric <- function(x, ini = 1, ...,
  t_end = 100, res = 0.01)
{
  half_life = x
  k = log(2)/half_life
  t_out <- seq(0, t_end, by = res)
  raw <- matrix(ini * exp( - k * t_out), ncol = 1)
  dimnames(raw) <- list(NULL, "parent")
  result <- ts(raw, 0, t_end, frequency = 1/res)
  class(result) <- c("one_box", "ts")
  return(result)
}

#' @rdname one_box
#' @param parms A named numeric vector containing the model parameters
#' @export
one_box.character <- function(x, ini = 1, parms, ...,
  t_end = 100, res = 0.01)
{
  parent_models_available = c("SFO", "FOMC", "DFOP", "HS", "SFORB", "IORE")
  if (length(x) == 1 & x %in% parent_models_available) {
    m <- mkinmod(parent = mkinsub(x))
  } else {
    stop("If you specify the decline model using a character string, ",
         "x has to be one of\n ",
          paste(parent_models_available, collapse = ", "))
  }

  if (!setequal(names(parms), m$par)) {
    stop("Please supply the parameters\n",
         paste(m$par, collapse = ", "))
  }

  t_out <- seq(0, t_end, by = res)
  pred <- mkinpredict(m, odeparms = parms, odeini = c(parent = ini),
                      outtimes = t_out, solution_type = "analytical")[-1]
  result <- ts(pred, 0, t_end, frequency = 1/res)
  class(result) <- c("one_box", "ts")
  return(result)
}

#' @rdname one_box
#' @importFrom mkin mkinpredict
#' @export
one_box.mkinfit <- function(x, ini = "model", ..., t_end = 100, res = 0.01) {
  fit <- x
  if (ini[1] == "model") {
    odeini = x$bparms.state
  } else {
    if (!is.numeric(ini[1])) stop ("Argument ini can only be 'model' or numeric")
    if (length(ini) == 1) odeini <- c(ini[1], rep(0, length(fit$mkinmod$spec) - 1))
    else odeini = ini
    names(odeini) <- names(fit$mkinmod$spec)
  }

  t_out = seq(0, t_end, by = res)
  if (length(fit$mkinmod$spec) == 1) solution_type = "analytical"
  else solution_type = "deSolve"

  tmp <- mkinpredict(fit$mkinmod, odeparms = fit$bparms.ode, odeini = odeini,
    outtimes = t_out, solution_type = solution_type)[-1]
  result <- ts(tmp, 0, t_end, frequency = 1/res)
  class(result) <- c("one_box", "ts")
  return(result)
}

#' Plot time series of decline data
#'
#' @param x The object of type \code{\link{one_box}} to be plotted
#' @param xlim Limits for the x axis
#' @param ylim Limits for the y axis
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param max_twa If a numeric value is given, the maximum time weighted
#'   average concentration(s) is/are shown in the graph.
#' @param max_twa_var Variable for which the maximum time weighted average should
#'   be shown if max_twa is not NULL.
#' @param ... Further arguments passed to methods
#' @importFrom stats plot.ts
#' @seealso \code{\link{sawtooth}}
#' @export
#' @examples
#' dfop_pred <- one_box("DFOP", parms = c(k1 = 0.2, k2 = 0.02, g = 0.7))
#' plot(dfop_pred)
#' plot(sawtooth(dfop_pred, 3, 7), max_twa = 21)
#'
#' # Use a fitted mkinfit model
#' m_2 <- mkinmod(parent = mkinsub("SFO", "m1"), m1 = mkinsub("SFO"))
#' fit_2 <- mkinfit(m_2, FOCUS_2006_D, quiet = TRUE)
#' pred_2 <- one_box(fit_2, ini = 1)
#' pred_2_saw <- sawtooth(pred_2, 2, 7)
#' plot(pred_2_saw, max_twa = 21, max_twa_var = "m1")
plot.one_box <- function(x,
                         xlim = range(time(x)), ylim = c(0, max(x)),
                         xlab = "Time", ylab = "Residue",
                         max_twa = NULL, max_twa_var = dimnames(x)[[2]][1], ...)
{
  obs_vars <- dimnames(x)[[2]]
  plot.ts(x, plot.type = "single", xlab = xlab, ylab = ylab,
          lty = 1:length(obs_vars), col = 1:length(obs_vars),
          las = 1, xlim = xlim, ylim = ylim)
  if (!is.null(max_twa)) {
    x_twa <- max_twa(x, window = max_twa)
    value <- x_twa$max[max_twa_var]
    rect(x_twa$window_start[max_twa_var], 0,
         x_twa$window_end[max_twa_var], value, col = "grey")
    text(x_twa$window_end[max_twa_var], value, paste("Maximum:", signif(value, 3)), pos = 4)
    # Plot a second time to cover the grey rectangle
    matlines(time(x), as.matrix(x), lty = 1:length(obs_vars), col = 1:length(obs_vars))
  }
}

#' Create decline time series for multiple applications
#'
#' If the number of application cycles \code{n} is greater than 1, the
#' application pattern specified in \code{applications} is repeated \code{n}
#' times, with an interval \code{i}.
#' @param x A \code{\link{one_box}} object
#' @param n The number of applications. If \code{applications} is specified, \code{n} is ignored
#' @param i The interval between applications. If \code{applications} is specified, \code{i}
#'   is ignored
#' @param applications A data frame holding the application times in the first column and
#'   the corresponding amounts applied in the second column.
#' @export
#' @examples
#' applications = data.frame(time = seq(0, 14, by = 7), amount = c(1, 2, 3))
#' pred <- one_box(10)
#' plot(sawtooth(pred, applications = applications))
#'
#' m_2 <- mkinmod(parent = mkinsub("SFO", "m1"), m1 = mkinsub("SFO"))
#' fit_2 <- mkinfit(m_2, FOCUS_2006_D, quiet = TRUE)
#' pred_2 <- one_box(fit_2, ini = 1)
#' pred_2_saw <- sawtooth(pred_2, 2, 7)
#' plot(pred_2_saw, max_twa = 21, max_twa_var = "m1")
#'
#' max_twa(pred_2_saw)
sawtooth <- function(x, n = 1, i = 365,
                     applications = data.frame(time = seq(0, 0 + n * i, length.out = n),
                                               amount = 1))
{
  n_obs = ncol(as.matrix(x))
  t_end = max(time(x))
  freq = frequency(x)
  empty <- ts(matrix(0, nrow = t_end * freq, ncol = n_obs), 0, t_end, freq)
  result <- empty
  for (i_app in 1:nrow(applications)) {
    t_app <- applications[i_app, "time"]
    amount_app <- applications[i_app, "amount"]
    if (t_app == 0) {
      result <- result + x * amount_app
    } else {
      lag_phase <- as.matrix(empty)[1:(t_app * freq), , drop = FALSE]
      app_phase <- amount_app * as.matrix(x)[1:((t_end - t_app) * freq + 1), , drop = FALSE]
      app_ts <- ts(rbind(lag_phase, app_phase), 0, t_end, frequency = freq)
      result <- result + app_ts
    }
  }
  class(result) = c("one_box", "ts")
  dimnames(result) <- dimnames(x)
  return(result)
}

#' Calculate a time weighted average concentration
#'
#' The moving average is built only using the values in the past, so
#' the earliest possible time for the maximum in the time series returned
#' is after one window has passed.
#'
#' @param x An object of type \code{\link{one_box}}
#' @param window The size of the moving window
#' @seealso \code{\link{max_twa}}
#' @importFrom stats start end
#' @export
#' @examples
#' pred <- sawtooth(one_box(10),
#'   applications = data.frame(time = c(0, 7), amount = c(1, 1)))
#' max_twa(pred)
twa <- function(x, window = 21) UseMethod("twa")

#' @rdname twa
#' @export
twa.one_box <- function(x, window = 21)
{
  length_ts <- end(x) - start(x)
  if (window >= length_ts[1]) {
    stop("The window must be smaller than the length of the time series")
  }

  resolution = 1/frequency(x)
  n_filter = window/resolution
  result = filter(x, rep(1/n_filter, n_filter), method = "convolution", sides = 1)
  class(result) = c("one_box", "ts")
  dimnames(result) <- dimnames(x)
  return(result)
}

#' The maximum time weighted average concentration for a moving window
#'
#' If you generate your time series using \code{\link{sawtooth}},
#' you need to make sure that the length of the time series allows
#' for finding the maximum. It is therefore recommended to check this using
#' \code{\link{plot.one_box}} using the window size for the argument
#' \code{max_twa}.
#' 
#' The method working directly on fitted \code{\link{mkinfit}} objects uses the
#' equations given in the PEC soil section of the FOCUS guidance and is restricted
#' SFO, FOMC and DFOP models and to the parent compound
#' @references FOCUS (2006) \dQuote{Guidance Document on Estimating Persistence and
#'   Degradation Kinetics from Environmental Fate Studies on Pesticides in EU
#'   Registration} Report of the FOCUS Work Group on Degradation Kinetics,
#'   EC Document Reference Sanco/10058/2005 version 2.0, 434 pp,
#'   \url{http://esdac.jrc.ec.europa.eu/projects/degradation-kinetics}
#' @seealso \code{\link{twa}}
#' @inheritParams twa
#' @export
#' @examples
#' pred <- sawtooth(one_box(10),
#'   applications = data.frame(time = c(0, 7), amount = c(1, 1)))
#' max_twa(pred)
#' pred_FOMC <- mkinfit("FOMC", FOCUS_2006_C, quiet = TRUE)
#' max_twa(pred_FOMC)
max_twa <- function(x, window = 21) UseMethod("max_twa")

#' @export
max_twa.mkinfit <- function(x, window = 21) {
  fit <- x
  parms.all <- c(fit$bparms.optim, fit$bparms.fixed)
  obs_vars <- fit$obs_vars
  if (length(obs_vars) > 1) {
    warning("Calculation of maximum time weighted average concentrations is",
            "currently only implemented for the parent compound using",
            "analytical solutions")
  }
  obs_var <- obs_vars[1]
  spec = fit$mkinmod$spec
  type = spec[[1]]$type

  M0 <- parms.all[paste0(obs_var, "_0")]

  if (type == "SFO") {
    k_name <- paste0("k_", obs_var)
    if (fit$mkinmod$use_of_ff == "min") {
      k_name <- paste0(k_name, "_sink")
    }
    k <- parms.all[k_name]
    twafunc <- function(t) {
      M0 * (1 - exp(- k * t)) / (k * t)
    }
  }
  if (type == "FOMC") {
    alpha <- parms.all["alpha"]
    beta <- parms.all["beta"]
    twafunc <- function(t) {
      M0 * (beta)/(t * (1 - alpha)) * ((t/beta + 1)^(1 - alpha) - 1)
    }
  }
  if (type == "DFOP") {
    k1 <- parms.all["k1"]
    k2 <- parms.all["k2"]
    g <- parms.all["g"]
    twafunc <- function(t) {
      M0/t * ((g/k1) * (1 - exp(- k1 * t)) + ((1 - g)/k2) * (1 - exp(- k2 * t)))
    }
  }
  if (type %in% c("HS", "IORE", "SFORB")) {
    stop("Calculation of maximum time weighted average concentrations is currently ",
         "not implemented for the ", type, " model.")
  }
  res <- twafunc(t = window)
  names(res) <- window
  return(res)
}

#' @export
max_twa.one_box <- function(x, window = 21)
{
  freq = frequency(x)

  twa_ts <- twa(x, window = window)
  window_end <- apply(twa_ts, 2, which.max) / freq
  result <- list()
  result$max <- apply(twa_ts, 2, max, na.rm = TRUE)
  result$window_start <- window_end - window
  result$window_end <- window_end
  return(result)
}
