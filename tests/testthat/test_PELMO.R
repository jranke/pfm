library(testthat)
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
    psm = "Pesticide_D_1_May_every_other_year_mets",
    mai = c("Cha")))


test_that("PELMO paths are correctly created", {
  psm_paths = c(
     PELMO_path(runs[[1]]$psm, "fbe", "Por"),
     PELMO_path(runs[[2]]$psm, "pot", "Ham"),
     PELMO_path(runs[[3]]$psm, "mai", "Cha"))

  # Check for psm files and put them into PELMO_base
  psm_new_locations <- character(0)
  for (i in seq_along(psm_paths)) {
    psm_file <- file.path(test_dir, psm_paths[i], paste0(runs[[i]]$psm, ".psm"))
    expect_true(file.exists(psm_file))
    psm_new_location <- file.path(PELMO_base, basename(psm_file))
    psm_new_locations[i] <- psm_new_location
    file.copy(psm_file, psm_new_location)
  }
})

test_that("PELMO runs are correctly set up", {

  # Prepare runs in analogy to the test archive
  PELMO_runs(runs, psm_dir = PELMO_base, execute = FALSE, overwrite = TRUE)

  # Check that input files are correctly generated in the right location
  for (run in runs) {
    psm <- run$psm
    # message(psm)
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      # message(crop)
      for (scenario in run[[crop]]) {
        # message(scenario)
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
  run_PELMO(runs, cores = 5)

  plm_files <- c("CHEM.PLM", "ECHO.PLM",
                 "KONZCHEM.PLM", "KONZC_A1", "KONZC_B1",
                 "PLNTPEST.plm", "PLOT.PLM", "WASSER.PLM")

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
          if (file.exists(file.path(test_dir, pp, plm))) {
            new <- readLines(file.path(PELMO_base, "FOCUS", pp, plm))
            test <- readLines(file.path(test_dir, pp, plm))

            # Check if the ouput files are correctly reproduced
            expect_identical(new, test)
          }
        }
      }
    }
  }
})

test_that("PELMO runs are correctly evaluated", {
  evaluate_PELMO(runs, psm_dir = PELMO_base)


})

# Clean up
unlink(psm_new_locations)
