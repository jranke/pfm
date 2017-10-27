library(pfm)
context("Exposit runoff calculations")

test_that("Runoff PECsw are as in Exposit 3.01", {
  # Expected results are from the Exposit 3.01, downloaded 2017-10-27

  # 100 g/ha, Koc = 1000 L/kg, DT50 = 1000 days
  PEC_spreadsheet_1 <- data.frame(dissolved = c(0.706, 0.612, 0.525, 0.367),
                                  total = c(0.783, 0.679, 0.546, 0.377))

  PEC_pfm_1 <- round(PEC_sw_exposit_runoff(100, Koc = 1000, DT50 = 1000)$PEC_sw_runoff, 3)[c("dissolved", "total")]
  expect_equivalent(PEC_spreadsheet_1, PEC_pfm_1)

  # 10 g/ha, Koc = 300000 L/kg, DT50 = 10 days
  PEC_spreadsheet_2 <- data.frame(dissolved = c(0, 0, 0, 0),
                                  total = c(0.141, 0.122, 0.039, 0.018))
  PEC_pfm_2 <- round(PEC_sw_exposit_runoff(10, Koc = 300000, DT50 = 10)$PEC_sw_runoff, 3)[c("dissolved", "total")]
  expect_equivalent(PEC_spreadsheet_2, PEC_pfm_2)

  # 200 g/ha, Koc = 30 L/kg, DT50 = 100 days
  PEC_spreadsheet_3 <- data.frame(dissolved = c(1.138, 0.986, 0.845, 0.592),
                                  total = c(1.138, 0.986, 0.845, 0.592))
  PEC_pfm_3 <- round(PEC_sw_exposit_runoff(200, Koc = 30, DT50 = 100)$PEC_sw_runoff, 3)[c("dissolved", "total")]
  expect_equivalent(PEC_spreadsheet_3, PEC_pfm_3)
})
