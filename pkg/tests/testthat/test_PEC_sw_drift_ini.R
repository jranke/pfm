library(pfm)
context("Simple PEC surface water calculations with drift entry")

test_that("PEC_sw_drift gives the same results as the CRD PEC calculator", {
  # One application of 30 g/ha to field crops calculated with UK PEC calculator published by CRD
  expect_equal(round(PEC_sw_drift_ini(30), 3), 
               c('1 m' = 0.277, '5 m' = 0.057, '10 m' = 0.029, '20 m' = 0.015))

  # 7 applications of 30 g/ha to field crops calculated with UK PEC calculator, initial PEC
  expect_equal(round(PEC_sw_drift_ini(30, 7), 3), 
               c('1 m' = 0.161, '5 m' = 0.033, '10 m' = 0.017, '20 m' = 0.008))

  # 4 applications of 30 g/ha to late fruit crops calculated with UK PEC calculator published by CRD (uses different drift values from SANCO aquatic guidance)
  #expect_equal(round(PEC_sw_drift(30, 4, crop = "Obstbau spät", distances = c(3, 20, 50)), 3), c('3 m' = 1.101, '20 m' = 0.080, '50 m' = 0.013))
})
