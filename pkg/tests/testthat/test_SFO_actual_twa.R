library(pfm)
context("Actual and time weighted average concentrations for SFO kinetics")

test_that("SFO_actual_twa calculates correctly", {
  test_times <- c(0, 1, 7, 21, 42)
  # This was calculated with the CRD spreadsheet for multiple applications
  reference <- data.frame(
    actual = c(10, 9.330, 6.156, 2.333, 0.544),
    twa = c(NaN, 9.661, 7.923, 5.267, 3.248),
    row.names = test_times)
  result <- round(10 * SFO_actual_twa(10, times = test_times), 3)
  expect_equal(result, reference)
})
