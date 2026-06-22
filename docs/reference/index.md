# Package index

## General utility functions

Functions that are independent of specific fate modelling areas

- [`geomean()`](https://pkgdown.jrwb.de/pfm/reference/geomean.md) :
  Calculate the geometric mean
- [`one_box()`](https://pkgdown.jrwb.de/pfm/reference/one_box.md) :
  Create a time series of decline data
- [`plot(`*`<one_box>`*`)`](https://pkgdown.jrwb.de/pfm/reference/plot.one_box.md)
  : Plot time series of decline data
- [`sawtooth()`](https://pkgdown.jrwb.de/pfm/reference/sawtooth.md) :
  Create decline time series for multiple applications
- [`twa()`](https://pkgdown.jrwb.de/pfm/reference/twa.md) : Calculate a
  time weighted average concentration
- [`max_twa()`](https://pkgdown.jrwb.de/pfm/reference/max_twa.md) : The
  maximum time weighted average concentration for a moving window
- [`pfm_degradation()`](https://pkgdown.jrwb.de/pfm/reference/pfm_degradation.md)
  : Calculate a time course of relative concentrations based on an
  mkinmod model
- [`SFO_actual_twa()`](https://pkgdown.jrwb.de/pfm/reference/SFO_actual_twa.md)
  : Actual and maximum moving window time average concentrations for SFO
  kinetics
- [`FOMC_actual_twa()`](https://pkgdown.jrwb.de/pfm/reference/FOMC_actual_twa.md)
  : Actual and maximum moving window time average concentrations for
  FOMC kinetics
- [`reexports`](https://pkgdown.jrwb.de/pfm/reference/reexports.md)
  [`set_nd_nq`](https://pkgdown.jrwb.de/pfm/reference/reexports.md)
  [`set_nd_nq_focus`](https://pkgdown.jrwb.de/pfm/reference/reexports.md)
  : Objects exported from other packages
- [`TSCF()`](https://pkgdown.jrwb.de/pfm/reference/TSCF.md) : Estimation
  of the transpiration stream concentration factor

## Predicted environmental concentrations in soil

- [`PEC_soil()`](https://pkgdown.jrwb.de/pfm/reference/PEC_soil.md) :
  Calculate predicted environmental concentrations in soil
- [`PEC_soil_mets()`](https://pkgdown.jrwb.de/pfm/reference/PEC_soil_mets.md)
  : Calculate initial and accumulation PEC soil for a set of metabolites
- [`soil_scenario_data_EFSA_2015`](https://pkgdown.jrwb.de/pfm/reference/soil_scenario_data_EFSA_2015.md)
  : Properties of the predefined scenarios from the EFSA guidance from
  2015
- [`soil_scenario_data_EFSA_2017`](https://pkgdown.jrwb.de/pfm/reference/soil_scenario_data_EFSA_2017.md)
  : Properties of the predefined scenarios from the EFSA guidance from
  2017
- [`PEC_FOMC_accu_rel()`](https://pkgdown.jrwb.de/pfm/reference/PEC_FOMC_accu_rel.md)
  : Get the relative accumulation of an FOMC model over multiples of an
  interval
- [`EFSA_washoff_2017`](https://pkgdown.jrwb.de/pfm/reference/EFSA_washoff_2017.md)
  : Subset of EFSA crop washoff default values

## Predicted environmental concentrations in groundwater

- [`FOCUS_GW_scenarios_2012`](https://pkgdown.jrwb.de/pfm/reference/FOCUS_GW_scenarios_2012.md)
  : A very small subset of the FOCUS Groundwater scenario definitions
- [`EFSA_GW_interception_2014`](https://pkgdown.jrwb.de/pfm/reference/EFSA_GW_interception_2014.md)
  : Subset of EFSA crop interception default values for groundwater
  modelling

## Predicted environmental concentrations in surface water

- [`PEC_sw_drift()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_drift.md)
  : Calculate predicted environmental concentrations in surface water
  due to drift
- [`drift_data_JKI`](https://pkgdown.jrwb.de/pfm/reference/drift_data_JKI.md)
  : Deposition from spray drift expressed as percent of the applied dose
  as published by the JKI
- [`drift_parameters_focus`](https://pkgdown.jrwb.de/pfm/reference/drift_parameters_focus.md)
  : Regression parameters for the Rautmann drift data
- [`drift_percentages_rautmann()`](https://pkgdown.jrwb.de/pfm/reference/drift_percentages_rautmann.md)
  : Calculate drift percentages based on Rautmann data
- [`PEC_sw_drainage_UK()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_drainage_UK.md)
  [`drainage_date_UK()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_drainage_UK.md)
  : Calculate initial predicted environmental concentrations in surface
  water due to drainage using the UK method
- [`PEC_sw_sed()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_sed.md)
  : Calculate predicted environmental concentrations in sediment from
  surface water concentrations
- [`PEC_sw_focus()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_focus.md)
  : Calculate PEC surface water at FOCUS Step 1
- [`chent_focus_sw()`](https://pkgdown.jrwb.de/pfm/reference/chent_focus_sw.md)
  : Create a chemical compound object for FOCUS Step 1 calculations
- [`FOCUS_Step_12_scenarios`](https://pkgdown.jrwb.de/pfm/reference/FOCUS_Step_12_scenarios.md)
  : Step 1/2 scenario data as distributed with the FOCUS Step 1/2
  calculator
- [`PEC_sw_exposit_drainage()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_exposit_drainage.md)
  : Calculate PEC surface water due to drainage as in Exposit 3
- [`PEC_sw_exposit_runoff()`](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_exposit_runoff.md)
  : Calculate PEC surface water due to runoff and erosion as in Exposit
  3
- [`perc_runoff_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_exposit.md)
  : Runoff loss percentages as used in Exposit 3
- [`perc_runoff_reduction_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_reduction_exposit.md)
  : Runoff reduction percentages as used in Exposit
- [`TOXSWA_cwa`](https://pkgdown.jrwb.de/pfm/reference/TOXSWA_cwa.md) :
  R6 class for holding TOXSWA water concentration data and associated
  statistics
- [`read.TOXSWA_cwa()`](https://pkgdown.jrwb.de/pfm/reference/read.TOXSWA_cwa.md)
  : Read TOXSWA surface water concentrations
- [`plot(`*`<TOXSWA_cwa>`*`)`](https://pkgdown.jrwb.de/pfm/reference/plot.TOXSWA_cwa.md)
  : Plot TOXSWA surface water concentrations

## Classifications and indicators

Evaluating environmental fate properties

- [`SSLRC_mobility_classification()`](https://pkgdown.jrwb.de/pfm/reference/SSLRC_mobility_classification.md)
  : Determine the SSLRC mobility classification for a chemical substance
  from its Koc
- [`GUS()`](https://pkgdown.jrwb.de/pfm/reference/GUS.md)
  [`print(`*`<GUS_result>`*`)`](https://pkgdown.jrwb.de/pfm/reference/GUS.md)
  : Groundwater ubiquity score based on Gustafson (1989)

## Work with chent objects containing relevant information

- [`endpoint()`](https://pkgdown.jrwb.de/pfm/reference/endpoint.md)
  [`soil_DT50()`](https://pkgdown.jrwb.de/pfm/reference/endpoint.md)
  [`soil_Kfoc()`](https://pkgdown.jrwb.de/pfm/reference/endpoint.md)
  [`soil_N()`](https://pkgdown.jrwb.de/pfm/reference/endpoint.md)
  [`soil_sorption()`](https://pkgdown.jrwb.de/pfm/reference/endpoint.md)
  : Retrieve endpoint information from the chyaml field of a chent
  object

## Utilities

- [`get_vertex()`](https://pkgdown.jrwb.de/pfm/reference/get_vertex.md)
  : Fit a parabola through three points
