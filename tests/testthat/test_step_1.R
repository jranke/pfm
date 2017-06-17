context("FOCUS Step 1 calculations")

t_out <- c(0, 1, 2, 4) # Checking the first four days is sufficient for Step 1
test_txt <- readLines(
  system.file("testdata/Steps_12_pesticide.txt", package = "pfm")
)

test_that("Results of Steps 1/2 calculator for Dummy 1 are reproduced", {
  dummy_1 <- chent_focus_sw("Dummy 1", cwsat = 6000, DT50_ws = 6, Koc = 344.8)
  res_dummy_1 <- PEC_sw_focus(dummy_1, 3000,
    comment = "Potatoes, Southern Europe, spring, 1 app/season, soil incorporation",
    f_drift = 0,
    append = FALSE, overwrite = TRUE)

  pest_txt <- readLines("pesticide.txt")
  expect_equal(test_txt[1], pest_txt[1])
  strsplit(test_txt[2], "\t")[[1]]

  PEC_orig_1 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_1[, "PECsw"]  = c(685.06, 610.32, 543.73, 431.56)
  PEC_orig_1[, "TWAECsw"]  = c(NA, 647.69, 612.03, 548.76)
  PEC_orig_1[, "PECsed"]  = c(2.36, 2.1, 1.87, 1.49) * 1e3
  PEC_orig_1[, "TWAECsed"]  = c(NA, 2.23e3, 2.11e3, 1.89e3)

  expect_equal(res_dummy_1$PEC[1:4, c(1, 2)], PEC_orig_1[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_1$PEC[1:4, c(3, 4)], PEC_orig_1[, c(3, 4)], tolerance = 10, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 2 are reproduced", {
  dummy_2 <- chent_focus_sw(cwsat = 30, DT50_ws = 26, Koc = 110)
  res_dummy_2 <- PEC_sw_focus(dummy_2, 1000)

  PEC_orig_2 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_2[, "PECsw"]  = c(299.89, 290.86, 283.21, 268.50)
  PEC_orig_2[, "TWAECsw"]  = c(NA, 295.38, 291.20, 283.49)
  PEC_orig_2[, "PECsed"]  = c(319.77, 319.95, 311.53, 295.35)
  PEC_orig_2[, "TWAECsed"]  = c(NA, 319.86, 317.79, 310.58)

  expect_equal(res_dummy_2$PEC[1:4, ], PEC_orig_2[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 4 are reproduced", {
  dummy_4 <- chent_focus_sw(cwsat = 2e-3, DT50_ws = 4, Koc = 970)
  res_dummy_4 <- PEC_sw_focus(dummy_4, 7.5, n = 3, i = 14,
                              scenario = "pome / stone fruit, early")

  PEC_orig_4 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_4[, "PECsw"]  = c(1.82, 1.18, 1.00, 0.70)
  PEC_orig_4[, "TWAECsw"]  = c(NA, 1.50, 1.29, 1.07)
  PEC_orig_4[, "PECsed"]  = c(10.57, 11.49, 9.66, 6.83)
  PEC_orig_4[, "TWAECsed"]  = c(NA, 11.03, 10.79, 9.48)

  expect_equal(res_dummy_4$PEC[1:4, ], PEC_orig_4[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 5 are reproduced", {
  dummy_5 <- chent_focus_sw(cwsat = 1.15, DT50_ws = 118, Koc = 860)
  res_dummy_5 <- PEC_sw_focus(dummy_5, 75, n = 5, i = 14,
                              scenario = "vines, early")

  PEC_orig_5 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_5[, "PECsw"]  = c(61.60, 59.45, 59.10, 58.41)
  PEC_orig_5[, "TWAECsw"]  = c(NA, 60.53, 59.90, 59.33)
  PEC_orig_5[, "PECsed"]  = c(500.78, 511.28, 508.29, 502.35)
  PEC_orig_5[, "TWAECsed"]  = c(NA, 506.03, 507.90, 506.61)

  expect_equal(res_dummy_5$PEC[1:4, ], PEC_orig_5[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 7 are reproduced", {
  dummy_7 <- chent_focus_sw(cwsat = 2.60, DT50_ws = 28, Koc = 500)
  res_dummy_7 <- PEC_sw_focus(dummy_7, 750, n = 4, i = 14,
                              scenario = "vines, early")

  PEC_orig_7 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_7[, "PECsw"]  = c(626.99, 601.13, 586.43, 558.10)
  PEC_orig_7[, "TWAECsw"]  = c(NA, 614.06, 603.90, 588.03)
  PEC_orig_7[, "PECsed"]  = c(3.0, 3.01, 2.93, 2.79) * 1e3
  PEC_orig_7[, "TWAECsed"]  = c(NA, 3.01e3, 2.99e3, 2.92e3)

  expect_equal(res_dummy_7$PEC[1:4, c(1, 2)], PEC_orig_7[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_7$PEC[1:4, c(3, 4)], PEC_orig_7[, c(3, 4)], tolerance = 10, scale = 1)
})

test_that("Results of Steps 1/2 calculator for New Dummy (M1-M3) are reproduced", {
  new_dummy <- chent_focus_sw(mw = 250, Koc = 100)
  M1 <- chent_focus_sw(mw = 100, cwsat = 100, DT50_ws = 100, Koc = 50, max_ws = 0, max_soil = 0.5)
  res_M1 <- PEC_sw_focus(new_dummy, 1000, scenario = "cereals, winter",
                         met = M1)

  PEC_orig_M1 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_M1[, "PECsw"]  = c(62.5, 62.07, 61.64, 60.79)
  PEC_orig_M1[, "TWAECsw"]  = c(NA, 62.28, 62.07, 61.64)
  PEC_orig_M1[, "PECsed"]  = c(31.25, 31.03, 30.82, 30.40)
  PEC_orig_M1[, "TWAECsed"]  = c(NA, 31.14, 31.03, 30.82)

  expect_equal(res_M1$PEC[1:4, ], PEC_orig_M1[, ], tolerance = 0.01, scale = 1)

  M2 <- chent_focus_sw(mw = 100, cwsat = 100, DT50_ws = 100, Koc = 50, max_ws = 0.5, max_soil = 0)
  res_M2 <- PEC_sw_focus(new_dummy, 1000, scenario = "cereals, winter",
                         met = M2)

  PEC_orig_M2 = matrix(NA, nrow = length(t_out), ncol = 4,
    dimnames = list(Time = t_out, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

  PEC_orig_M2[, "PECsw"]  = c(64.34, 63.78, 63.34, 62.47)
  PEC_orig_M2[, "TWAECsw"]  = c(NA, 64.06, 63.81, 63.36)
  PEC_orig_M2[, "PECsed"]  = c(31.25, 31.89, 31.67, 31.23)
  PEC_orig_M2[, "TWAECsed"]  = c(NA, 31.57, 31.68, 31.56)

  expect_equal(res_M2$PEC[1:4, ], PEC_orig_M2[, ], tolerance = 0.01, scale = 1)
})
unlink("pesticide.txt")
