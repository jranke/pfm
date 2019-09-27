library(pfm)
context("UK drainage PEC calculations")

test_that("The mobility classification and the drained percentage are correct", {
  # Expected results are from the CRD drainage calculator, retrieved 2019-09-27
  # The last test result was 0.01 when this test was written 2015-06-11.
  # I assume that the value of 0.008 was displayed as 0.01 by the spreadsheet
  # available at that time
  
  expect_equivalent(SSLRC_mobility_classification(1), list("Very mobile", 1.9))
  expect_equivalent(SSLRC_mobility_classification(15), list("Mobile", 1.9))
  expect_equivalent(SSLRC_mobility_classification(30), list("Mobile", 1.9))
  expect_equivalent(SSLRC_mobility_classification(74.9), list("Mobile", 1.9))
  expect_equivalent(SSLRC_mobility_classification(75), list("Moderately mobile", 0.7))
  expect_equivalent(SSLRC_mobility_classification(100), list("Moderately mobile", 0.7))
  expect_equivalent(SSLRC_mobility_classification(800), list("Slightly mobile", 0.5))
  expect_equivalent(SSLRC_mobility_classification(2000), list("Slightly mobile", 0.02))
  expect_equivalent(SSLRC_mobility_classification(5000), list("Non mobile", 0.008))
})

test_that("UK drainflow PECs are correct", {
  # Expected results are from the CRD drainage calculator, retrieved 2015-06-11, except
  # for the third example from the data requirements handbook

  # This is the first example calculation from the data requirements handbook, where they give
  # 8.07 µg/L as the result (obviously a rounding error).
  expect_equal(round(PEC_sw_drainage_UK(150, interception = 0, Koc = 100), 4), 8.0769)

  # This is the second example calculation from the data requirements handbook
  expect_equal(round(PEC_sw_drainage_UK(90, interception = 0, Koc = 10), 4), 13.1538)

  # This is the third example calculation from the data requirements handbook, 
  expect_equal(round(PEC_sw_drainage_UK(60, interception = 0.5, Koc = 550,
                                        latest_application = "01 July",
                                        soil_DT50 = 200), 2), 0.84)
})
