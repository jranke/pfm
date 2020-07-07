context("Read and analyse TOXSWA cwa files")

#   zipfile_test = tempfile()
#   download.file("https://cgit.jrwb.de/pfm/plain/inst/testdata/SwashProjects.zip", zipfile_test)
zipfile_test <- system.file("testdata/SwashProjects.zip", package = "pfm")
basedir_test = "SwashProjects/project_H_sw/TOXSWA" # cwa files from TOXSWA 3.x

# .out file produced with TOXSWA 4.4.2
basedir_test_2 = "SwashProjects/Project_1/TOXSWA"

H_sw_D4_pond  <- read.TOXSWA_cwa("00001p_pa.cwa",
  basedir = basedir_test,
  zipfile = zipfile_test)

H_sw_R1_stream  <- read.TOXSWA_cwa("00003s_pa.cwa",
  basedir = basedir_test,
  zipfile = zipfile_test)

test_that("Old TOXSWA cwa and out files are correctly read and printed", {

  # This was the setting when printing the output into text files
  options(width = 100)

  # Most content of the R6 object is at least partially printed

  H_sw_D4_pond_printed <- capture.output(print(H_sw_D4_pond))

  expect_equal(H_sw_D4_pond_printed, readLines("H_sw_D4_pond_printed.txt"))

  H_sw_R1_stream_printed <- capture.output(print(H_sw_R1_stream))
  expect_equal(H_sw_R1_stream_printed, readLines("H_sw_R1_stream_printed.txt"))

  # The basedir is not printed, therefore tested separately
  expect_equal(H_sw_D4_pond$basedir, basedir_test)


  EXSW2_R1_stream  <- read.TOXSWA_cwa("3.out",
                                 basedir = basedir_test_2,
                                 zipfile = zipfile_test)
  EXSW2_R1_stream_printed <- capture.output(print(EXSW2_R1_stream))
  expect_equal(EXSW2_R1_stream_printed, readLines("EXSW2_R1_stream_printed.txt"))
})

test_that("A TOXSWA 5.5.3 out file is correctly read and printed", {
  # zipfile_test_3 = tempfile()
  # download.file("https://cgit.jrwb.de/pfm/plain/inst/testdata/SwashProjects_TOXSWA_553.zip",
  #   zipfile_test_3)
  zipfile_test_3 <- system.file("testdata/SwashProjects_TOXSWA_553.zip", package = "pfm")
  basedir_test_3 = "SwashProjects/Project_1/TOXSWA"

  EXSW2_R1_stream_TOXSWA_553  <- read.TOXSWA_cwa("3.out",
                                  basedir = basedir_test_3,
                                  zipfile = zipfile_test_3)
  EXSW2_R1_stream_printed_TOXSWA_553 <- capture.output(print(EXSW2_R1_stream_TOXSWA_553))

  # We actually get the same results, at least judged by the printed object
  expect_equal(EXSW2_R1_stream_printed_TOXSWA_553, readLines("EXSW2_R1_stream_printed.txt"))
})

test_that("Getting events and moving window analysis works", {
  # Event analysis with two different thresholds
  H_sw_R1_stream$get_events(c(2, 10))
  expect_known_output(H_sw_R1_stream$events, file = "H_sw_R1_stream_events.txt")

  # Moving window analysis
  H_sw_R1_stream$moving_windows(c(7, 21))
  H_sw_R1_stream$windows
  expect_known_output(H_sw_R1_stream$windows, file = "H_sw_R1_stream_windows.txt")
})
