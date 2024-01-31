context("FOCUS Step 1 calculations") # {{{1

test_txt <- readLines(
  system.file("testdata/Steps_12_pesticide.txt", package = "pfm")
)

# Define test compounds as in pesticide.txt
dummy_1 <- chent_focus_sw("Dummy 1", cwsat = 6000, DT50_ws = 6, DT50_soil = 6, Koc = 344.8,
                          DT50_water = 6, DT50_sediment = 6)
dummy_2 <- chent_focus_sw("Dummy 2", cwsat = 30, DT50_ws = 26, DT50_soil = 56, Koc = 110,
                          DT50_water = 26, DT50_sediment = 26)
dummy_4 <- chent_focus_sw("Dummy 4", cwsat = 2e-3, DT50_ws = 4, Koc = 970,
                          DT50_soil = 19, DT50_water = 4, DT50_sediment = 4)
dummy_5 <- chent_focus_sw("Dummy 5", cwsat = 1.15, DT50_ws = 118, Koc = 860,
                          DT50_soil = 250, DT50_water = 6, DT50_sediment = 118)
dummy_7 <- chent_focus_sw("Dummy 7", cwsat = 2.60, DT50_ws = 28, Koc = 500,
                          DT50_soil = 50, DT50_water = 2.5, DT50_sediment = 28)
new_dummy <- chent_focus_sw("New Dummy", mw = 250, Koc = 100,
                            DT50_soil = 10)
M1 <- chent_focus_sw("Metabolite M1",
                     mw = 100, cwsat = 100, DT50_ws = 100,
                     Koc = 50, max_ws = 0, max_soil = 0.5,
                     DT50_soil = 20, DT50_water = 10, DT50_sediment = 100)
M2 <- chent_focus_sw("Metabolite M2",
                     mw = 100, cwsat = 100, DT50_ws = 100,
                     Koc = 50, max_ws = 0.5, max_soil = 0,
                     DT50_soil = 20, DT50_water = 10, DT50_sediment = 100)

t_out_1 <- c(0, 1, 2, 4) # Checking the first four days is sufficient for Step 1
PEC_template_1 <- matrix(NA, nrow = length(t_out_1), ncol = 4,
    dimnames = list(Time = t_out_1, type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed")))

t_out_2 <- c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100) # We read in text from rtf reports for Step 2

test_that("Results of Steps 1/2 calculator for Dummy 1 are reproduced", { # {{{1
  res_step_1_1 <- PEC_sw_focus(dummy_1, 3000,
    comment = "Potatoes, Southern Europe, spring, 1 app/season, soil incorporation",
    scenario = "no drift (incorp or seed trtmt)",
    region = "s", season = "mm",
    append = FALSE, overwrite = TRUE)

  PEC_step_1_1 <- PEC_template_1
  PEC_step_1_1[, "PECsw"]  = c(685.06, 610.32, 543.73, 431.56)
  PEC_step_1_1[, "TWAECsw"]  = c(NA, 647.69, 612.03, 548.76)
  PEC_step_1_1[, "PECsed"]  = c(2.36, 2.1, 1.87, 1.49) * 1e3
  PEC_step_1_1[, "TWAECsed"]  = c(NA, 2.23e3, 2.11e3, 1.89e3)

  expect_equal(res_step_1_1$PEC[1:4, c(1, 2)], PEC_step_1_1[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_step_1_1$PEC[1:4, c(3, 4)], PEC_step_1_1[, c(3, 4)], tolerance = 10, scale = 1)

  # This is pasted from the file "Dummy 1 step 2.rtf" generated with Steps12 version 3.2 from 15/05/2017
  PEC_step_2_1_raw <- read.table(text = "0 172.6235  NA 595.2057  NA
1 153.7900  163.2067  530.2680  562.7368
2 137.0113  154.3037  472.4151  532.0392
4 108.7460  138.3873  374.9561  477.1595
7  76.8950  118.5090  265.1340  408.6191
14   34.2528   85.6494  118.1038  295.3191
21   15.2579   64.9380   52.6092  223.9062
28    6.7966   51.3222   23.4348  176.9589
42    1.3486   35.3389    4.6500  121.8484
50    0.5352   29.8256    1.8454  102.8388
100   0.0017   14.9591    0.0057   51.5788")
  PEC_step_2_1 = PEC_step_2_1_raw[, 2:5]
  dimnames(PEC_step_2_1) = list(Time = t_out_2,
    type = c("PECsw", "TWAECsw", "PECsed", "TWAECsed"))

  # Step 2 is not implemented, so this can not be tested.
})

test_that("Results of Steps 1/2 calculator for Dummy 2 are reproduced", { # {{{1
  res_dummy_2 <- PEC_sw_focus(dummy_2, 1000,
    comment = "Maize, Southern Europe, spring, 1 app/season",
    scenario = "maize",
    region = "s", season = "mm",
    append = TRUE)

  PEC_step_1_2 = PEC_template_1

  PEC_step_1_2[, "PECsw"]  = c(299.89, 290.86, 283.21, 268.50)
  PEC_step_1_2[, "TWAECsw"]  = c(NA, 295.38, 291.20, 283.49)
  PEC_step_1_2[, "PECsed"]  = c(319.77, 319.95, 311.53, 295.35)
  PEC_step_1_2[, "TWAECsed"]  = c(NA, 319.86, 317.79, 310.58)

  expect_equal(res_dummy_2$PEC[1:4, ], PEC_step_1_2[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 4 are reproduced", { # {{{1
  res_dummy_4 <- PEC_sw_focus(dummy_4, 7.5, n = 3, i = 14,
    comment = "Apples, Southern  Europe, spring, 3 app./season, 14 d int, orchards",
    region = "s", season = "mm",
    scenario = "pome / stone fruit, early",
    append = TRUE)

  PEC_step_1_4 = PEC_template_1

  PEC_step_1_4[, "PECsw"]  = c(1.82, 1.18, 1.00, 0.70)
  PEC_step_1_4[, "TWAECsw"]  = c(NA, 1.50, 1.29, 1.07)
  PEC_step_1_4[, "PECsed"]  = c(10.57, 11.49, 9.66, 6.83)
  PEC_step_1_4[, "TWAECsed"]  = c(NA, 11.03, 10.79, 9.48)

  expect_equal(res_dummy_4$PEC[1:4, ], PEC_step_1_4[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 5 are reproduced", { # {{{1
  res_dummy_5 <- PEC_sw_focus(dummy_5, 75, n = 5, i = 14,
    comment = "Vines, Northern Europe, spring, 5 app/seaon 14 d int.",
    region = "n", season = "mm",
    scenario = "vines, early",
    append = TRUE)

  PEC_step_1_5 = PEC_template_1

  PEC_step_1_5[, "PECsw"]  = c(61.60, 59.45, 59.10, 58.41)
  PEC_step_1_5[, "TWAECsw"]  = c(NA, 60.53, 59.90, 59.33)
  PEC_step_1_5[, "PECsed"]  = c(500.78, 511.28, 508.29, 502.35)
  PEC_step_1_5[, "TWAECsed"]  = c(NA, 506.03, 507.90, 506.61)

  expect_equal(res_dummy_5$PEC[1:4, ], PEC_step_1_5[, ], tolerance = 0.01, scale = 1)
})

test_that("Results of Steps 1/2 calculator for Dummy 7 are reproduced", { # {{{1
  res_dummy_7 <- PEC_sw_focus(dummy_7, 750, n = 4, i = 14,
    comment = "Vines, Southern Europe, spring, 4 app/seaon 14 d int.",
    region = "s", season = "mm",
    scenario = "vines, early",
    append = TRUE)

  PEC_step_1_7 = PEC_template_1

  PEC_step_1_7[, "PECsw"]  = c(626.99, 601.13, 586.43, 558.10)
  PEC_step_1_7[, "TWAECsw"]  = c(NA, 614.06, 603.90, 588.03)
  PEC_step_1_7[, "PECsed"]  = c(3.0, 3.01, 2.93, 2.79) * 1e3
  PEC_step_1_7[, "TWAECsed"]  = c(NA, 3.01e3, 2.99e3, 2.92e3)

  expect_equal(res_dummy_7$PEC[1:4, c(1, 2)], PEC_step_1_7[, c(1, 2)], tolerance = 0.01, scale = 1)
  expect_equal(res_dummy_7$PEC[1:4, c(3, 4)], PEC_step_1_7[, c(3, 4)], tolerance = 10, scale = 1)
})

test_that("Results of Steps 1/2 calculator for New Dummy (M1-M2) are reproduced", { # {{{1
  res_M1 <- PEC_sw_focus(new_dummy, 1000, scenario = "cereals, winter",
    comment = "Soil Metabolite",
    region = "n", season = "of",
    met = M1,
    append = TRUE)

  PEC_step_1_M1 = PEC_template_1

  PEC_step_1_M1[, "PECsw"]  = c(62.5, 62.07, 61.64, 60.79)
  PEC_step_1_M1[, "TWAECsw"]  = c(NA, 62.28, 62.07, 61.64)
  PEC_step_1_M1[, "PECsed"]  = c(31.25, 31.03, 30.82, 30.40)
  PEC_step_1_M1[, "TWAECsed"]  = c(NA, 31.14, 31.03, 30.82)

  expect_equal(res_M1$PEC[1:4, ], PEC_step_1_M1[, ], tolerance = 0.01, scale = 1)

  res_M2 <- PEC_sw_focus(new_dummy, 1000, scenario = "cereals, winter",
    comment = "Water Metabolite",
    region = "n", season = "of",
    met = M2,
    append = TRUE)

  PEC_step_1_M2 = PEC_template_1

  PEC_step_1_M2[, "PECsw"]  = c(64.34, 63.78, 63.34, 62.47)
  PEC_step_1_M2[, "TWAECsw"]  = c(NA, 64.06, 63.81, 63.36)
  PEC_step_1_M2[, "PECsed"]  = c(31.25, 31.89, 31.67, 31.23)
  PEC_step_1_M2[, "TWAECsed"]  = c(NA, 31.57, 31.68, 31.56)

  expect_equal(res_M2$PEC[1:4, ], PEC_step_1_M2[, ], tolerance = 0.01, scale = 1)
})

context("FOCUS Steps 12 input files") # {{{1
# When we compare the generated input file with the test file,
# we can ignore some fields if we are looking at the parent ai
# Also, the ai and compound names are not checked, as we append
# scenario, region and season in order to get unique names
# for Step 2 result files of the Step12 calculator
field_index <- c(ai = 1, compound = 2, comment = 3,
  mw_ai = 4, mw_met = 5,
  cwsat = 6, Koc_assessed = 7,
  Koc_parent = 8,
  DT50_ws = 9,
  max_ws = 10, max_soil = 11,
  rate = 12, n = 13, i = 14, app_type = 15,
  DT50_soil_parent = 16, DT50_soil = 17, DT50_water = 18, DT50_sediment = 19,
  reg_sea = 20, int_class = 21)
field_index_mets <- field_index[-c(1, 2)]
field_index_parent <- field_index[-c(1:2, 4:5, 8, 10:11, 16)]

test_that("Runs are correctly defined in the Steps 12 input file", { # {{{1

  pest_txt <- readLines("pesticide.txt")
  expect_equal(test_txt[1], pest_txt[1]) # Header

  # Dummy 1
  test_1 <- strsplit(test_txt[2], "\t")[[1]][field_index_parent]
  pest_1 <- strsplit(pest_txt[2], "\t")[[1]][field_index_parent]
  expect_equal(test_1, pest_1) # Parent fields

  # Dummy 2
  test_2 <- strsplit(test_txt[3], "\t")[[1]][field_index_parent]
  pest_2 <- strsplit(pest_txt[3], "\t")[[1]][field_index_parent]
  expect_equal(test_2, pest_2) # Parent fields

  # Dummy 4
  test_4 <- strsplit(test_txt[5], "\t")[[1]][field_index_parent]
  pest_4 <- strsplit(pest_txt[4], "\t")[[1]][field_index_parent]
  expect_equal(test_4, pest_4) # Parent fields

  # Dummy 5
  test_5 <- strsplit(test_txt[6], "\t")[[1]][field_index_parent]
  pest_5 <- strsplit(pest_txt[5], "\t")[[1]][field_index_parent]
  expect_equal(test_5, pest_5) # Parent fields

  # Dummy 7
  test_7 <- strsplit(test_txt[9], "\t")[[1]][field_index_parent]
  pest_7 <- strsplit(pest_txt[6], "\t")[[1]][field_index_parent]
  expect_equal(test_7, pest_7) # Parent fields

  # New Dummy / M1
  test_m1 <- strsplit(test_txt[10], "\t")[[1]][field_index_mets]
  pest_m1 <- strsplit(pest_txt[7], "\t")[[1]][field_index_mets]
  expect_equal(test_m1, pest_m1) # All fields except ai and met names

  # New Dummy / M2
  test_m2 <- strsplit(test_txt[11], "\t")[[1]][field_index_mets]
  pest_m2 <- strsplit(pest_txt[8], "\t")[[1]][field_index_mets]
  expect_equal(test_m2, pest_m2) # All fields except ai and met names

})
unlink("pesticide.txt")
# vim: set foldmethod=marker: {{{
