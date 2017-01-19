library(pfm)
context("Check max_twa for parent mkinfit models against analytical solutions")

test_that("max_twa for simple decline curves is correct", {
  fits <- mmkin(c("SFO", "FOMC", "DFOP"), list(FOCUS_2006_C), cores = 1, quiet = TRUE)
  max_twa_21_analytical <- sapply(fits, max_twa, 21)
  names(max_twa_21_analytical) <- rep("parent", 3)
  preds <- lapply(fits, one_box)
  max_twa_21_preds <- sapply(preds, function(x) max_twa(x, 21)[["max"]])
  expect_equal(max_twa_21_analytical, max_twa_21_preds, tolerance = .03, scale = 1)
})
