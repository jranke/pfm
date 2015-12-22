library(pfm)
context("Simple PEC soil calculations")

test_that("PEC_soil calculates correctly", {
  # Application of 100 g/ha gives 0.133 mg/kg under default assumptions
  expect_equal(as.numeric(PEC_soil(100)), 0.1 * 4/3)

  # or 0.1 mg/kg assuming 25% interception
  expect_equal(as.numeric(PEC_soil(100, interception = 0.25)), 0.1) 

  # Mixing depth of 1 cm gives five-fold PEC
  expect_equal(as.numeric(PEC_soil(100, interception = 0.25, mixing_depth = 1)), 0.5)
})

test_that("Tier 1 PEC soil example for Pesticide A in EFSA guidance can be reproduced", {
  # Calculate total soil concentrations for tier 1 scenarios
  results_pfm <- PEC_soil(1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
                          scenarios = "EFSA_2015")

  # From Table I.5, p. 80
  results_guidance <- matrix(c(22.0, 11.5, 9.1, 21.8, 11.4, 9.0), 
                             ncol = 3, byrow = TRUE)
  dimnames(results_guidance) <- list(t_avg = c(0, 21), 
                                     scenario = c("CTN", "CTC", "CTS"))


  expect_equal(round(results_pfm, 1), results_guidance)

  # Calculate porewater soil concentrations for tier 1 scenarios
  results_pfm_pw <- PEC_soil(1000, interval = 365, DT50 = 250, t_av = c(0, 21),
                             Kom = 1000, scenarios = "EFSA_2015", porewater = TRUE)

  # From Table I.5, p. 80
  results_guidance_pw <- matrix(c(0.76, 0.67, 0.91, 0.75, 0.66, 0.90), 
                                ncol = 3, byrow = TRUE)
  dimnames(results_guidance_pw) <- list(t_avg = c(0, 21), 
                                        scenario = c("CLN", "CLC", "CLS"))

  expect_equal(round(results_pfm_pw, 2), results_guidance_pw)
})

test_that("Tier 1 PEC soil example for Pesticide F in EFSA guidance can be reproduced", {
  # Parent F
  # Calculate total and porewater soil concentrations for tier 1 scenarios
  results_pfm <- PEC_soil(1000, interval = 365, DT50 = 25, t_avg = c(0, 21),
                          scenarios = "EFSA_2015")
  results_pfm_pw <- PEC_soil(1000, interval = 365, DT50 = 25, t_av = c(0, 21),
                             Kom = 1000, scenarios = "EFSA_2015", porewater = TRUE)

  # From Table I.14, p. 88
  results_guidance <- matrix(c(12.8, 7.7, 6.6, 11.8, 6.8, 5.7),
                             ncol = 3, byrow = TRUE)
  results_guidance_pw <- matrix(c(0.50, 0.46, 0.71, 0.45, 0.41, 0.60),
                                ncol = 3, byrow = TRUE)

  # Skip checking dimnames by using expect_equivalent()
  expect_equivalent(round(results_pfm, 1), results_guidance)
  expect_equivalent(round(results_pfm_pw, 2), results_guidance_pw)

  # Metabolite M1
  # Calculate total and porewater soil concentrations for tier 1 scenarios
  # Relative molar mass is 200/300, formation fraction is 0.7
  results_pfm <- PEC_soil(200/300 * 0.7 * 1000, interval = 365, DT50 = 100, t_avg = c(0, 21),
                          scenarios = "EFSA_2015")
  results_pfm_pw <- PEC_soil(200/300 * 0.7 * 1000, interval = 365, DT50 = 100, t_av = c(0, 21),
                             Kom = 10, scenarios = "EFSA_2015", porewater = TRUE)

  # From Table I.15, p. 88
  results_guidance <- matrix(c(7.27, 4.08, 3.38, 7.12, 3.97, 3.26),
                             ncol = 3, byrow = TRUE)
  results_guidance_pw <- matrix(c(12.93, 10.42, 11.66, 12.58, 10.09, 11.15),
                                ncol = 3, byrow = TRUE)

  # Skip checking dimnames by using expect_equivalent()
  expect_equivalent(round(results_pfm, 2), results_guidance)
  expect_equivalent(round(results_pfm_pw, 2), results_guidance_pw)

  # Metabolite M2
  # Calculate total and porewater soil concentrations for tier 1 scenarios
  # Relative molar mass is 100/300, formation fraction is 0.7 * 1
  results_pfm <- PEC_soil(100/300 * 0.7 * 1 * 1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
                          scenarios = "EFSA_2015")
  results_pfm_pw <- PEC_soil(100/300 * 0.7 * 1000, interval = 365, DT50 = 250, t_av = c(0, 21),
                             Kom = 100, scenarios = "EFSA_2015", porewater = TRUE)

  # From Table I.16, p. 89
  results_guidance <- matrix(c(5.13, 2.69, 2.13, 5.08, 2.66, 2.10),
                             ncol = 3, byrow = TRUE)
  results_guidance_pw <- matrix(c(1.61, 1.39, 1.80, 1.60, 1.37, 1.77),
                                ncol = 3, byrow = TRUE)

  # Skip checking dimnames by using expect_equivalent()
  expect_equivalent(round(results_pfm, 2), results_guidance)
  expect_equivalent(round(results_pfm_pw, 2), results_guidance_pw)
})
