library(pfm)
context("Simple PEC soil calculations")

test_that("PEC_soil calculates correctly", {
  # Application of 100 g/ha gives 0.133 mg/kg under default assumptions
  expect_equal(PEC_soil(100), 0.1 * 4/3)

  # or 0.1 mg/kg assuming 25% interception
  expect_equal(PEC_soil(100, interception = 0.25), 0.1) 

  # Mixing depth of 1 cm gives five-fold PEC
  expect_equal(PEC_soil(100, interception = 0.25, mixing_depth = 1), 0.5)
})
