context("Processing of residue series")
# FOCUS (2014) page 76 (parent) and page 132 (metabolite)

parent_1 <- c(.12, .09, .05, .03, "nd", "nd", "nd", "nd", "nd", "nd")
parent_2 <- c(.12, .09, .05, .03, "nd", "nd", .03, "nd", "nd", "nd")
parent_3 <- c(.12, .09, .05, .03, "nd", "nd", .06, "nd", "nd", "nd")
metabolite <- c("nd", "nd", "nd", 0.03, 0.06, 0.10, 0.11, 0.10, 0.09, 0.05, 0.03, "nd", "nd")

test_that("Simple residue series processed as intended", {

  expect_equal(set_nd(parent_1, 0.02),
    c(.12, .09, .05, .03, .01, rep(NA, 5)))

  expect_equal(set_nd(parent_2, 0.02, loq = 0.05),
    c(.12, .09, .05, .03, .01, .01, .03, .01, NA, NA))

  expect_equal(set_nd(metabolite, 0.02, loq = 0.05),
    c(NA, NA, .01, .03, .06, .1, .11, .1, .09, .05, .03, .01, NA))

})

test_that("Simple residue series are processed as in the FOCUS guidance", {

  # Parent 1
  expect_error(set_nd_focus(parent_1, 0.02),
    "You need to specify an loq")
  expect_equal(set_nd_focus(parent_1, 0.02, 0.05),
    c(.12, .09, .05, .03, .01, rep(NA, 5)))

  # Parent 2
  expect_equal(set_nd_focus(parent_2, 0.02, loq = 0.05),
    c(.12, .09, .05, .03, .01, rep(NA, 5)))

  # Parent 3
  expect_equal(set_nd_focus(parent_3, 0.02, loq = 0.05),
    c(.12, .09, .05, .03, .01, .01, .06, .01, NA, NA))

  # Metabolite
  expect_equal(set_nd_focus(metabolite, 0.02, loq = 0.05),
    c(0, NA, .01, .03, .06, .1, .11, .1, .09, .05, .03, .01, NA))

})
