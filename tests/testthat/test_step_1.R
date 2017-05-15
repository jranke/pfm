context("FOCUS Step 1 calculations")

test_that("Results of Steps 1/2 calculator for Dummy 1 are reproduced", {
  dummy_1 <- chent_focus_sw(cwsat = 6000, DT50_ws = 6, Koc = 344.8)
  res_dummy_1 <- PEC_sw_focus(dummy_1, 3000, f_drift = 0)

  t_out <- c(0, 1, 2, 4) # Checking the first four days should be sufficient for Step 1
  PEC_orig = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_1[, "PECsw"]  = c(685.06, 610.32, 543.73, 431.56)
  PEC_orig_1["1", "TWAECsw"]  = 647.69 # Later TWAEC not implemented
  PEC_orig_1[, "PECsed"]  = c(2.36, 2.1, 1.87, 1.49) * 1e3
  PEC_orig_1["1", "TWAECsed"]  = 2.23e3 # Later TWAEC not implemented

  expect_equal(res_dummy_1$PEC[1:4, c(1, 2)], PEC_orig_1[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_1$PEC[1:4, c(3, 4)], PEC_orig_1[, c(3, 4)], tolerance = 10, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 2 are reproduced", {
  dummy_2 <- chent_focus_sw(cwsat = 30, DT50_ws = 26, Koc = 110)
  res_dummy_2 <- PEC_sw_focus(dummy_2, 1000)

  t_out <- c(0, 1, 2, 4) # Checking the first four days should be sufficient for Step 1
  PEC_orig_2 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_2[, "PECsw"]  = c(299.89, 290.86, 283.21, 268.50)
  PEC_orig_2["1", "TWAECsw"]  = 295.38 # Later TWAEC not implemented
  PEC_orig_2[, "PECsed"]  = c(319.77, 319.95, 311.53, 295.35)
  PEC_orig_2["1", "TWAECsed"]  = 319.86 # Later TWAEC not implemented

  expect_equal(res_dummy_2$PEC[1:4, c(1, 2)], PEC_orig_2[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_2$PEC[1:4, c(3, 4)], PEC_orig_2[, c(3, 4)], tolerance = 10, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 2 are reproduced", {
  dummy_2 <- chent_focus_sw(cwsat = 30, DT50_ws = 26, Koc = 110)
  res_dummy_2 <- PEC_sw_focus(dummy_2, 1000)

  t_out <- c(0, 1, 2, 4) # Checking the first four days should be sufficient for Step 1
  PEC_orig_2 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_2[, "PECsw"]  = c(299.89, 290.86, 283.21, 268.50)
  PEC_orig_2["1", "TWAECsw"]  = 295.38 # Later TWAEC not implemented
  PEC_orig_2[, "PECsed"]  = c(319.77, 319.95, 311.53, 295.35)
  PEC_orig_2["1", "TWAECsed"]  = 319.86 # Later TWAEC not implemented

  expect_equal(res_dummy_2$PEC[1:4, c(1, 2)], PEC_orig_2[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_2$PEC[1:4, c(3, 4)], PEC_orig_2[, c(3, 4)], tolerance = 10, scale = 1)
})
