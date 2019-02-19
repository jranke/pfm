library(pfm)
context("Exposit calculations")
# Expected results are from the Exposit 3.02, downloaded 2019-02-15

test_that("Runoff PECsw are as in Exposit 3.02", {
  # 100 g/ha, Koc = 1000 L/kg, DT50 = 1000 days
  res_exposit_1 <- data.frame(
    runoff = c(183.62, 110.17, 73.45, 36.72),
    erosion = c(19.96, 11.98, 2.99, 1.00),
    PEC_dissolved = c(0.71, 0.61, 0.52, 0.37),
    PEC_total = c(0.78, 0.68, 0.55, 0.38))

  res_1 <- PEC_sw_exposit_runoff(100, Koc = 1000, DT50 = 1000)
  res_pfm_1 <- data.frame(
    runoff = round(1000 * res_1$runoff["dissolved"], 2),
    erosion = round(1000 * res_1$runoff["bound"], 2),
    PEC_dissolved = round(res_1$PEC_sw_runoff, 2)["dissolved"],
    PEC_total = round(res_1$PEC_sw_runoff["dissolved"] + res_1$PEC_sw_runoff["bound"], 2))
  expect_equivalent(res_exposit_1, res_pfm_1)

  # 10 g/ha, Koc = 300000 L/kg, DT50 = 10 days
  res_exposit_2 <- data.frame(
    runoff = c(0.08, 0.05, 0.03, 0.02),
    erosion = c(36.63, 21.98, 5.49, 1.83),
    PEC_dissolved = c(0, 0, 0, 0),
    PEC_total = c(0.14, 0.12, 0.04, 0.02))

  res_2 <- PEC_sw_exposit_runoff(10, Koc = 300000, DT50 = 10)
  res_pfm_2 <- data.frame(
    runoff = round(1000 * res_2$runoff["dissolved"], 2),
    erosion = round(1000 * res_2$runoff["bound"], 2),
    PEC_dissolved = round(res_2$PEC_sw_runoff, 2)["dissolved"],
    PEC_total = round(res_2$PEC_sw_runoff["dissolved"] + res_2$PEC_sw_runoff["bound"], 2))

  expect_equivalent(res_exposit_2, res_pfm_2)

  # 200 g/ha, Koc = 30 L/kg, DT50 = 100 days
  res_exposit_3 <- data.frame(
    runoff = c(295.78, 177.47, 118.31, 59.16),
    erosion = rep(0.00, 4),
    PEC_dissolved = c(1.14, 0.99, 0.85, 0.59),
    PEC_total = c(1.14, 0.99, 0.85, 0.59))

  res_3 <- PEC_sw_exposit_runoff(200, Koc = 30, DT50 = 100)
  res_pfm_3 <- data.frame(
    runoff = round(1000 * res_3$runoff["dissolved"], 2),
    erosion = round(1000 * res_3$runoff["bound"], 2),
    PEC_dissolved = round(res_3$PEC_sw_runoff, 2)["dissolved"],
    PEC_total = round(res_3$PEC_sw_runoff["dissolved"] + res_3$PEC_sw_runoff["bound"], 2))

  expect_equivalent(res_exposit_3, res_pfm_3)
})

test_that("Drainage PECsw are as in Exposit 3.02", {
  # 100 g/ha, Koc = 1000 L/kg, DT50 = 1000 days
  res_exposit_1 <- c(spring = 0.02, autumn = 0.05)

  res_1 <- PEC_sw_exposit_drainage(100, Koc = 1000, DT50 = 1000)
  res_pfm_1 <- round(res_1$PEC_sw_drainage, 2)
  expect_equivalent(res_exposit_1, res_pfm_1)

  # 10 g/ha, Koc = 300000 L/kg, DT50 = 10 days
  res_exposit_2 <- c(spring = 0.00, autumn = 0.00)

  res_2 <- PEC_sw_exposit_drainage(10, Koc = 300000, DT50 = 10)
  res_pfm_2 <- round(res_2$PEC_sw_drainage, 2)
  expect_equivalent(res_exposit_2, res_pfm_2)

  # 200 g/ha, Koc = 30 L/kg, DT50 = 100 days
  res_exposit_3 <- c(spring = 0.61, autumn = 1.88)

  res_3 <- PEC_sw_exposit_drainage(200, Koc = 30, DT50 = 100)
  res_pfm_3 <- round(res_3$PEC_sw_drainage, 2)
  expect_equivalent(res_exposit_3, res_pfm_3)

  # 1000 g/ha, Koc = 545 L/kg, group = 1, DT50 = 20 days, 25% interception
  res_exposit_4 <- c(spring = 0.11, autumn = 0.32)

  res_4 <- PEC_sw_exposit_drainage(1000, interception = 0.25, Koc = 545, DT50 =
                                   20, mobility = "low")
  res_pfm_4 <- round(res_4$PEC_sw_drainage, 2)
  expect_equivalent(res_exposit_4, res_pfm_4)
})
