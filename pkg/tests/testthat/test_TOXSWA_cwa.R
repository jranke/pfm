library(pfm)
context("Read and analyse TOXSWA cwa files")

test_that("TOXSWA cwa file is correctly read", {
  H_sw_D4_pond  <- read.TOXSWA_cwa("00001p_pa.cwa",
                                   basedir = "SwashProjects/project_H_sw/TOXSWA",
                                   zipfile = system.file("testdata/SwashProjects.zip",
                                               package = "pfm"))
  expect_equal_to_reference(H_sw_D4_pond, file = "H_sw_D4_pond.rds")
})

test_that("Getting events and moving window analysis works", {
  H_sw_R1_stream  <- read.TOXSWA_cwa("00003s_pa.cwa",
                                   basedir = "SwashProjects/project_H_sw/TOXSWA",
                                   zipfile = system.file("testdata/SwashProjects.zip",
                                               package = "pfm"))

  # Event analysis with two different thresholds
  H_sw_R1_stream$get_events(c(2, 10))
  expect_equal_to_reference(H_sw_R1_stream$events, file = "H_sw_R1_stream_events.rds")

  # Moving window analysis
  H_sw_R1_stream$moving_windows(c(7, 21))
  expect_equal_to_reference(H_sw_R1_stream$windows, file = "H_sw_R1_stream_windows.rds")
})
