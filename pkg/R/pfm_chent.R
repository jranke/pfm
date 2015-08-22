# Copyright (C) 2015  Johannes Ranke
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

#' R6 class for holding a chemical entity
#'
#' An R6 class for holding information about a chemical entity that is useful
#' for fate modelling
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @field acronym Acronym for local use
#' @field smiles SMILES code
#' @field mw Molecular weight
#' @field main_source Main source of information
#' @field transformations
#' @field sorption_endpoints
#' @examples
#' atrazine  <- pfm_chent("atrazine", smiles = "Clc1nc(nc(n1)NC(C)C)NCC", mw = 215.68)
#' print(atrazine)
#' @keywords data

pfm_chent <- R6Class("pfm_chent",
  public <- list(
    acronym = NULL,
    smiles = NULL,
    mw = NULL,
    main_source = NULL,
    initialize = function(acronym, smiles = NULL, mw = NULL, main_source = NULL) {
      self$acronym <- acronym
      self$smiles <- smiles
      self$mw <- mw
      self$main_source <- main_source
    },
    p0 = data.frame(p0 = numeric(0), T = numeric(0),
                    comment = character(0), 
                    source = character(0), pages = character(0),
                    stringsAsFactors = FALSE),
    add_p0 = function(p0, T = 25, comment = "", source, pages = NA) {
      i <- nrow(self$p0) + 1
      self$p0[i, c("p0", "T")] <- c(p0, T)
      self$p0[i, c("comment", "pages")] <- c(comment, pages)
      if (!missing(source)) self$p0[i, "source"] <- source
    },
    cwsat = data.frame(cwsat = numeric(0), T = numeric(0),
                       pH = numeric(0), comment = character(0),
                       source = character(0), pages = character(0),
                       stringsAsFactors = FALSE),
    add_cwsat = function(cwsat, T = NA, pH = NA, comment = "", source, pages = NA) {
      i <- nrow(self$cwsat) + 1
      self$cwsat[i, c("cwsat", "T", "pH")] <- c(cwsat, T, pH)
      self$cwsat[i, c("comment", "pages")] <- c(comment, pages)
      if (!missing(source)) self$cwsat[i, "source"] <- source
    },
    TPs = list(),
    add_TP = function(x, smiles = NULL, mw = NULL, main_source = NULL) {
      if (inherits(x, "pfm_chent")) {
        chent_name <- deparse(substitute(x))
        chent <- x
      } else {
        chent_name <- make.names(x)
        chent <- pfm_chent$new(x, smiles, mw, main_source) 
      }
      self$TPs[[chent_name]] <- chent
    },
    transformations = data.frame(study_type = character(0),
                                 acronym = character(0), 
                                 max_occurrence = numeric(0), 
                                 source = character(0), 
                                 pages = character(0),
                                 stringsAsFactors = FALSE),
    add_transformations = function(study_type, TPs, max_occurrence, 
                                   comment = "", source, pages = NA) {
      TP_names = make.names(TPs)
      TP_is_chent = sapply(self$TPs[TP_names], function(x) inherits(x, "pfm_chent"))
      if (!all(TP_is_chent)) {
        stop(paste("Please add all TPs using pfm_chent$add_TP()", print(TP_is_chent)))
      } 
      chents <- self$TPs[TP_names]
      if (missing(source)) source <- self$main_source
      if (is.numeric(pages)) pages <- paste(pages, collapse = ", ")
      transformations <- data.frame(study_type = study_type, 
                                    acronym = sapply(chents, function(x) x$acronym),
                                    max_occurrence = max_occurrence, 
                                    comment = comment,
                                    source = source, 
                                    pages = pages,
                                    row.names = NULL,
                                    stringsAsFactors = FALSE)
      self$transformations <- rbind(self$transformations, transformations)
    },
    soil_degradation_endpoints = data.frame(destination = character(0), 
                                            DT50 = numeric(0),
                                            comment = character(0),
                                            pages = character(0),
                                            stringsAsFactors = FALSE),
    add_soil_degradation_endpoints = function(destination, DT50 = NA,
                                              comment = "", pages = NA) {
      i <- nrow(self$soil_degradation_endpoints) + 1
      self$soil_degradation_endpoints[i, c("destination", "comment", "pages")] <- 
        c(destination, comment, pages)
      self$soil_degradation_endpoints[i, "DT50"] <- DT50
    },
    ws_degradation_endpoints = data.frame(destination = character(0), 
                                          DT50 = numeric(0),
                                          comment = character(0),
                                          tier = character(0),
                                          pages = character(0),
                                          stringsAsFactors = FALSE),
    add_ws_degradation_endpoints = function(destination, DT50 = NA, tier = NA,
                                            comment = "", pages = NA) {
      i <- nrow(self$ws_degradation_endpoints) + 1
      self$ws_degradation_endpoints[i, c("destination", "tier", "comment", "pages")] <- 
        c(destination, tier, comment, pages)
      self$ws_degradation_endpoints[i, "DT50"] <- DT50
    },
    water_degradation_endpoints = data.frame(destination = character(0), 
                                             DT50 = numeric(0),
                                             tier = character(0),
                                             comment = character(0),
                                             pages = character(0),
                                             stringsAsFactors = FALSE),
    add_water_degradation_endpoints = function(destination, DT50 = NA, tier = NA,
                                               comment = "", pages = NA) {
      i <- nrow(self$water_degradation_endpoints) + 1
      self$water_degradation_endpoints[i, c("destination", "tier", "comment", "pages")] <- 
        c(destination, tier, comment, pages)
      self$water_degradation_endpoints[i, "DT50"] <- DT50
    },
    sediment_degradation_endpoints = data.frame(destination = character(0), 
                                                DT50 = numeric(0),
                                                tier = character(0),
                                                comment = character(0),
                                                pages = character(0),
                                                stringsAsFactors = FALSE),
    add_sediment_degradation_endpoints = function(destination, DT50 = NA, tier = NA,
                                                  comment = "", pages = NA) {
      i <- nrow(self$sediment_degradation_endpoints) + 1
      self$sediment_degradation_endpoints[i, c("destination", "tier", "comment", "pages")] <- 
        c(destination, tier, comment, pages)
      self$sediment_degradation_endpoints[i, "DT50"] <- DT50
    },
    sorption_endpoints = data.frame(Kfoc = numeric(0), f1.n = numeric(0),
                                    type = character(0),
                                    comment = character(0),
                                    pages = character(0),
                                    stringsAsFactors = FALSE),
    add_sorption_endpoints = function(Kfoc, f1.n = 1, type = "arithmetic mean",
                                      comment = "", pages = NA) {
      i <- nrow(self$sorption_endpoints) + 1
      self$sorption_endpoints[i, c("Kfoc", "f1.n")] <- c(Kfoc, f1.n)
      self$sorption_endpoints[i, c("type", "comment", "pages")] <- c(type, comment, pages)
    },
    print = function(TPs = TRUE) {
      cat("<pfm_chent> with acronym", self$acronym, "\n")
      if (!is.null(self$smiles)) cat ("SMILES:", self$smiles, "\n")
      if (!is.null(self$mw)) cat ("Molecular weight:", round(self$mw, 1), "\n")
      if (!is.null(self$main_source)) cat ("Main source of information:", self$main_source, "\n")
      if (TPs) {
        if (length(self$TPs) > 0) {
          cat("\nTransformation products:\n")
          print(self$TPs)
        }
        if (nrow(self$transformations) > 0) {
          cat("\nTransformations:\n")
          print(self$transformations[order(self$transformations$study_type), ], row.names = FALSE)
        }
      }
    }
  )
)

#' R6 class for holding active ingredients
#'
#' An R6 class for holding information about active ingredients
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object.
#' @field iso ISO common name
#' @examples
#' atrazine  <- pfm_ai("atrazine", smiles = "Clc1nc(nc(n1)NC(C)C)NCC", mw = 215.68)
#' print(atrazine)
#' @keywords data

pfm_ai <- R6Class("pfm_ai",
  inherit = pfm_chent,
  public <- list(
    iso = NULL,
    initialize = function(iso, acronym = iso, smiles = NULL, mw = NULL, main_source = NULL) {
      super$initialize(acronym = acronym, smiles = smiles, mw = mw, main_source = main_source)
      self$iso <- iso
    },
    ff = data.frame(from = character(0), to = character(0), ff = numeric(0),
                    comment = character(0), pages = character(0),
                    stringsAsFactors = FALSE),
    add_ff = function(from = "parent", to, ff = 1, comment = "", pages = NA) {
      i <- nrow(self$ff) + 1
      if (from != "parent") {
        if (!exists(from, self$TPs)) stop(from, " was not found in TPs")
      }
      if (!exists(to, self$TPs)) stop(to, " was not found in TPs")
      self$ff[i, ] <- c(from, to, ff, comment, pages)
    },
    print = function() {
      if (is.null(self$iso)) cat("<pfm_ai> with acronym", self$acronym, "\n")
      else cat("<pfm_ai> with ISO common name", self$iso, "\n")
      super$print()
    }
  )
)

#' R6 class for holding a product with at least one active ingredient
#'
#' An R6 class for holding information about a product with at least one active ingredient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object.
#' @field name The name of the product
#' @field ais A list of active ingredients
#' @field concentrations The concentration of the ais
#' @field concentration_units Defaults to g/L
#' @keywords data

pfm_product <- R6Class("pfm_product",
  public <- list(
    name = NULL,
    ais = list(),
    concentrations = NULL,
    concentration_units = NULL,
    density = NULL,
    density_units = "g/L",
    initialize = function(name, ..., concentrations, concentration_units = "g/L",
                          density = 1000, density_units = "g/L") {
      self$name <- name
      self$ais <- list(...)
      self$concentrations <- concentrations
      self$density <- density
      self$density_units <- density_units
      names(self$concentrations) <- names(self$ais)
      self$concentration_units <- concentration_units
    },
    print = function() {
      cat("<pfm_product> named", self$name, "\n")
    }
  )
)

