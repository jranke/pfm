library(pfm)
library(testthat)
library(units)
context("Simple PEC surface water calculations with drift entry")

test_that("PEC_sw_drift gives the same results as the CRD PEC calculator", {
  # One application of 30 g/ha to field crops calculated with UK PEC calculator published by CRD
  expect_equal(
    round(PEC_sw_drift(30), 3),
    set_units(c('1 m' = 0.277, '5 m' = 0.057, '10 m' = 0.029, '20 m' = 0.015), "\u00B5g/L"))

  # 7 applications of 30 g/ha to field crops calculated with UK PEC calculator, initial PEC
  expect_equal(
    round(PEC_sw_drift(30, 7), 3),
    set_units(c('1 m' = 0.161, '5 m' = 0.033, '10 m' = 0.017, '20 m' = 0.008), "\u00B5g/L"))

  # 4 applications of 30 g/ha to late fruit crops calculated with UK PEC
  # calculator published by CRD. CRD uses different drift values from SANCO aquatic
  # guidance), except for 50 m
  pfm_30_4_obst_spaet <- round(PEC_sw_drift(30, 4, crop_group_JKI = "Obstbau spaet",
      distances = c(3, 20, 50)), 3)
  crd_30_4_obst_spaet <- set_units(c('3 m' = 1.101, '20 m' = 0.080, '50 m' = 0.013), "µg/L")
  expect_equal(pfm_30_4_obst_spaet[3], crd_30_4_obst_spaet[3])

  # Synops scenario with 45 m angle. Mean width is 100 cm - (2 * 15 cm).
  expect_equal(
    PEC_sw_drift(100) * 100/70,
    PEC_sw_drift(100, side_angle = 45)
  )
})

test_that("The Rautmann formula is correctly implemented", {
  pfm_jki <- PEC_sw_drift(100)
  pfm_rf <- PEC_sw_drift(100, drift_data = "RF")
  expect_equal(pfm_jki, pfm_rf, tolerance = 0.01)

  expect_error(PEC_sw_drift(100, drift_data = "RF", applications = 10), "Only 1 to 8 applications")
  expect_error(PEC_sw_drift(100, drift_data = "RF", applications = 1, crop_group_RF = "Obstbau spaet"),
    "should be one of")
  expect_silent(PEC_sw_drift(100, drift_data = "RF", applications = 1, crop_group_RF = "fruit, late"))
})
