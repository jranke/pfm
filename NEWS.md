## version 0.6.4

- R/PEC_sw_drift.R: Vectorise the function not only with respect to distances, rates and water depths, but also with respect to crop groups. Closes issue #2 reported by Julian Klein (@juklei).

## version 0.6.4

- R/PEC_sw_drainage_uk.R: Fix a bug preventing the function to work of `latest_application` is set to 29 February. Also, make this function correctly deal with units.

- R/twa.R: Fix a bug in plotting one-box models of class `one_box` that affected plots that displayed a time weighted average.

- R/PEC_sw_drainage_uk.R: Fix a bug leading to increased PEC values in the case the application date is after the beginning of the drainage period and `soil_DT50` was specified.


## version 0.6.3

- R/{PEC_sw_drift,PEC_sw_exposit_runoff,PEC_sw_sed}.R: Make use of the `units` package.

- R/PEC_sw_drift.R: Change argument name from 'crop_group_focus' to 'crop_group_RF', in order to make it easier to understand the relation to the 'drift_data' argument.
