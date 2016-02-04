library(pfm)
context("Read and analyse TOXSWA cwa files")

zipfile_test = system.file("testdata/SwashProjects.zip", package = "pfm")
basedir_test = "SwashProjects/project_H_sw/TOXSWA"

H_sw_D4_pond  <- read.TOXSWA_cwa("00001p_pa.cwa",
                                 basedir = basedir_test,
                                 zipfile = zipfile_test)

H_sw_R1_stream  <- read.TOXSWA_cwa("00003s_pa.cwa",
                                 basedir = basedir_test,
                                 zipfile = zipfile_test)

basedir_test_2 = "SwashProjects/Project_1/TOXSWA"

EXSW2_R1_stream  <- read.TOXSWA_cwa("3.out",
                                 basedir = basedir_test_2,
                                 zipfile = zipfile_test)



test_that("TOXSWA cwa file is correctly read and printed", {

  # This was the setting when printing the output into text files
  options(width = 100)

  # Most content of the R6 object is at least partially printed

  H_sw_D4_pond_printed <- capture.output(print(H_sw_D4_pond))

  expect_equal(H_sw_D4_pond_printed, readLines("H_sw_D4_pond_printed.txt"))

  H_sw_R1_stream_printed <- capture.output(print(H_sw_R1_stream))
  expect_equal(H_sw_R1_stream_printed, readLines("H_sw_R1_stream_printed.txt"))

  # The basedir is not printed, therefore tested separately
  expect_equal(H_sw_D4_pond$basedir, basedir_test)

  EXSW2_R1_stream_printed <- capture.output(print(EXSW2_R1_stream))
  expect_equal(EXSW2_R1_stream_printed, readLines("EXSW2_R1_stream_printed.txt"))

  # The basedir is not printed, therefore tested separately
  expect_equal(H_sw_D4_pond$basedir, basedir_test)
})

test_that("Getting events and moving window analysis works", {

  # Event analysis with two different thresholds
  H_sw_R1_stream$get_events(c(2, 10))
  expect_equal_to_reference(H_sw_R1_stream$events, file = "H_sw_R1_stream_events.rds")

  # Moving window analysis
  H_sw_R1_stream$moving_windows(c(7, 21))
  expect_equal_to_reference(H_sw_R1_stream$windows, file = "H_sw_R1_stream_windows.rds")
})
