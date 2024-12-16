## version 0.6.4

- R/PEC_sw_drainage_uk.R: Fix a bug leading to increased PEC values in the case the application date is after the beginning of the drainage period and `soil_DT50` was specified.

## version 0.6.3

- R/{PEC_sw_drift,PEC_sw_exposit_runoff,PEC_sw_sed}.R: Make use of the `units` package.

- R/PEC_sw_drift.R: Change argument name from 'crop_group_focus' to 'crop_group_RF', in order to make it easier to understand the relation to the 'drift_data' argument.
