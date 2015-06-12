context("Simple PEC sediment calculations")

test_that("PEC_sw_sed calculates correctly using the percentage method", {
  # Application of 100 g/ha, 1 m spray drift distance (2.77% drift input), 50% in sediment,
  # default assumptions of CRD spreadsheet (5 cm sediment depth, 1.3 kg/L sediment density)
  # Reference value calculated with CRD spreadsheet
  PEC_sw_100_1_m <- PEC_sw_drift_ini(100, distances = 1)
  expect_equivalent(round(PEC_sw_sed(PEC_sw_100_1_m, percentage = 50), 3), 2.131)
})
