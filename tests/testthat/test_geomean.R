library(pfm)
context("Geometric mean calculation")

test_that("The geometric mean is correctly calculated", {
  expect_equal(geomean(c(1, 3, 9)), 3)
  expect_equal(geomean(c(0, 3, 9)), 0)
  expect_equal(geomean(c(1, 3, NA, 9), na.rm = FALSE), NA)
  expect_equal(geomean(c(1, 3, NA, 9), na.rm = TRUE), 3)
  expect_error(geomean(c(1, -3, 9)), "positive")
  expect_equal(geomean(c(1, -3, NA, 9)), NA)
})
