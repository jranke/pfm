#' Regression parameters for the Rautmann drift data
#'
#' The parameters were extracted from Appendix B to the FOCUS surface water guidance
#' using the R code given in the file `data_generation/drift_parameters_focus.R`
#' installed with this package. The appendix itself is not included in the package,
#' as its licence is not clear.
#' 
#' For the hinge distance, `Inf` was substituted for the cases where no hinge
#' distance is given in the data, in this way parameters C and D are never
#' used for any distance if A and B are used for the case that the distance
#' is smaller than the hinge distance.
#'
#' @name drift_parameters_focus
#' @docType data
#' @seealso [drift_percentages_rautmann], [PEC_sw_drift]
#' @format A [tibble::tibble].
#' @references FOCUS (2014) Generic guidance for Surface Water Scenarios (version 1.4).
#' FOrum for the Co-ordination of pesticde fate models and their USe.
#' <http://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/Generic%20FOCUS_SWS_vc1.4.pdf>
#' 
#' FOCUS (2001) FOCUS Surface Water Scenarios in the EU Evaluation Process
#' under 91/414/EEC. Report of the FOCUS Working Group on Surface Water
#' Scenarios, EC Document Reference SANCO/4802/2001-rev.2. 245, Appendix B.
#' <https://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/FOCUS_SWS_APPENDIX_B.doc> 
#'
#' Rautmann, D., Streloke, M and Winkler, R (2001) New basic drift values in
#' the authorization procedure for plant protection products Mitt. Biol.
#' Bundesanst. Land- Forstwirtsch. 383, 133-141
#' @keywords datasets
#' @examples
#' drift_parameters_focus
#' unique(drift_parameters_focus$crop_group)
"drift_parameters_focus"
