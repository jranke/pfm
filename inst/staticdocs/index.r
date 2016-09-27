sd_section(
  "Degradation",
  "Functions related to degradation",
  c("pfm_degradation", "SFO_actual_twa")
)
sd_section(
  "Mobility",
  "Indicators related to mobility",
  c("GUS", "SSLRC_mobility_classification")
)
sd_section(
  "Soil",
  "Predicted environmental concentrations in soil",
  c("PEC_soil", "soil_scenario_data_EFSA_2015")
)
sd_section(
  "Surface water",
  "Predicted environmental concentrations in surface water",
  c(
    "PEC_sw_drift", "PEC_sw_sed",
    "drift_data_JKI",
    "PEC_sw_drainage_UK", 
    "TOXSWA_cwa",
    "read.TOXSWA_cwa", 
    "plot.TOXSWA_cwa"
    )
)
sd_section(
  "Groundwater",
  "Predicted environmental concentrations in groundwater",
  c("FOCUS_GW_scenarios_2012")
)
sd_section(
  "Chemical entities",
  "Work with input stored in chemical entites as R objects using the chent package",
  c("endpoint")
)
sd_section(
  "General",
  "Utilities that are generally useful",
  c("geomean")
)
