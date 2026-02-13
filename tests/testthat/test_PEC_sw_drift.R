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

  expect_error(PEC_sw_drift(100, drift_data = "RF", applications = 1, crop_group_RF = "Obstbau spaet"))
  expect_silent(PEC_sw_drift(100, drift_data = "RF", applications = 1, crop_group_RF = "fruit, late"))
})

test_that("The function is vectorised also with respect to crop groups", {
  res_vec_1 <- PEC_sw_drift(
    rate = rep(100, 6),
    applications = c(1, 2, rep(1, 4)),
    water_depth = c(30, 30, 30, 60, 30, 30),
    crop_group_JKI = c(rep("Ackerbau", 4), rep("Obstbau frueh", 2)),
    distances = c(rep(5, 4), 10, 5))
  expect_equal(
    round(res_vec_1, 3),
    set_units(c('5 m' = 0.190, '5 m' = 0.157, '5 m' = 0.190, '5 m' = 0.095, '10 m' = 3.937, '5 m' = 6.630), "\u00B5g/L"))

  # Try the same with the Rautmann formula, results are slightly different
  res_vec_2 <- PEC_sw_drift(
    rate = rep(100, 6),
    applications = c(1, 2, rep(1, 4)),
    water_depth = c(30, 30, 30, 60, 30, 30),
    drift_data = "RF",
    crop_group_RF = c(rep("arable", 4), rep("fruit, early", 2)),
    distances = c(rep(5, 4), 10, 5))
  expect_equal(
    round(res_vec_2, 3),
    set_units(c('5 m' = 0.191, '5 m' = 0.160, '5 m' = 0.191, '5 m' = 0.095, '10 m' = 3.936, '5 m' = 6.628), "\u00B5g/L"))

})
