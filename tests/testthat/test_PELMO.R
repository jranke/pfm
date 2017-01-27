# Remove when the test is ready to be run by testthat
#library(testthat)
#test_dir <- "/tmp/Rtmp8DFCT5"
#psm_name <- "D_rel_1"
#source("~/git/pfm/inst/extdata/FOCUS_PELMO_data.R")
#source("~/git/pfm/R/PELMO_runs.R")

library(pfm)
context("Create PELMO runs from psm files and execute them")
PELMO_base <- system.file("FOCUSPELMO.553", package = "PELMO.installeR")

test_archive <- system.file("testdata/FOCUS_PELMO.tar.bz2", package = "pfm")
test_dir <- tempdir()
untar(test_archive, exdir = test_dir, compressed = "bzip2")

runs <- list(
  list(
    psm = "Pesticide_D",
    fbe = c("Por"),
    vbe = c("Por")),
  list(
    psm = "Pesticide_D_1_day_pre_em_every_third_year",
    pot = c("Cha", "Ham")),
  list(
    psm = "Pesticide_D_1_May_every_other_year",
    mai = c("Cha")))

psm_paths = c(
   D_rel_1 = PELMO_path(runs[1]$psm, "fbe", "Por"),
   D_rel_3 = PELMO_path(runs[2]$psm, "pot", "Ham"),
   D_abs_2 = PELMO_path(runs[3]$psm, "mai", "Cha"))

# Get psm files and put them into PELMO_base
psm_new_locations <- character(0)
for (psm_name in names(psm_paths)) {
  psm_file <- file.path(test_dir, psm_paths[psm_name], paste0(runs[1]$psm, ".psm"))
  psm_new_location <- file.path(PELMO_base, basename(psm_file))
  psm_new_locations[psm_name] <- psm_new_location
  file.copy(psm_file, psm_new_location)
}

test_that("PELMO runs are correctly set up", {

  # Prepare runs in analogy to the test archive
  PELMO_runs(runs, psm_dir = PELMO_base, execute = FALSE, overwrite = TRUE)

  # Check that input files are correctly generated in the right location
  for (run in runs) {
    psm <- run$psm
    message(psm)
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
       message(crop)
      for (scenario in run[[crop]]) {
         message(scenario)
        pp <- PELMO_path(psm, crop, scenario)

        input_new <- readLines(file.path(PELMO_base, "FOCUS", pp, "pelmo.inp"))
        input_test <- readLines(file.path(test_dir, pp, "pelmo.inp"))

        # Check if the input files are correctly reproduced
        expect_identical(input_new, input_test)
      }
    }
  }
})

test_that("PELMO runs can be run and give the expected result files", {
  run_PELMO(runs, psm_dir = PELMO_base, cores = 5)

  plm_files <- c("CHEM.PLM", "ECHO.PLM", "KONZCHEM.PLM", "PLNTPEST.plm",
                 "PLOT.PLM", "WASSER.PLM")

  # Check that if output is the same as in the test archive
  for (run in runs) {
    psm <- run$psm
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      # message(crop)
      for (scenario in run[[crop]]) {
        # message(scenario)
        pp <- PELMO_path(psm, crop, scenario)

        for (plm in plm_files) {
          new <- readLines(file.path(PELMO_base, "FOCUS", pp, plm))
          test <- readLines(file.path(test_dir, pp, plm))

          # Check if the ouput files are correctly reproduced
          expect_identical(new, test)
        }
      }
    }
  }
})

test_that("PELMO runs are correctly evaluated", {

})

# Clean up
unlink(psm_new_locations)
