#' Set up runs for FOCUS PELMO
#'
#' Per default, the runs are also executed with FOCUS PELMO, and the results are processed
#' and returned.
#'
#' @param runs A list of lists. Each inner lists has an element named 'psm'
#'   that holds the psm string, and elements named using three letter crop acronyms,
#'   as used in \code{\link{FOCUS_PELMO_crops}},
#'   that hold character vectors of three letter scenario acronyms
#'   as used in \code{\link{FOCUS_GW_scenarios_2012}}.
#' @param psm_dir The directory where the psm files are located
#' @param version The FOCUS PELMO version
#' @param PELMO_base Where the FOCUS PELMO installation is located
#' @param execute Should PELMO be executed directly?
#' @param cores The number of cores to execute PELMO runs in parallel
#' @param evaluate Should the results be returned?
#' @param overwrite Should an existing run directories be overwritten
#' @export
PELMO_runs <- function(runs, psm_dir = ".", version = "5.5.3", PELMO_base = "auto",
                       execute = TRUE, cores = getOption("mc.cores", 2L),
                       evaluate = TRUE, overwrite = FALSE)
{
  if (PELMO_base[1] == "auto") {
    PELMO_base = file.path(system.file(package = "PELMO.installeR"),
                                       paste0("FOCUSPELMO.", gsub("\\.", "", version)))
  }
  PELMO_exe = list.files(PELMO_base, "^pelmo.*.exe", full.names = TRUE)
  if (length(PELMO_exe) == 0) {
    stop("Could not find PELMO executable. Did you run PELMO.installeR::install_PELMO()?")
  }

  run_list <- create_run_list(runs, psm_dir = psm_dir, check_psm_files = TRUE)

  setup_run <- function(x) {
    psm <- x$psm
    crop <- x$crop
    scenario <- x$scenario

    location_code <- FOCUS_PELMO_location_codes[scenario]

    run_dir <- file.path(PELMO_base, "FOCUS", PELMO_path(psm, crop, scenario))

    if (dir.exists(run_dir)) {
      if (overwrite) {
        unlink(run_dir, recursive = TRUE)
      } else {
        stop("Run directory for ", crop, " in ", scenario, "\n", run_dir, "\nalready exists")
      }
    }

    # Create the run directory
    dir.create(run_dir, recursive = TRUE)

    # Copy the psm file
    psm_file <- file.path(psm_dir, paste0(psm, ".psm"))
    file.copy(psm_file, run_dir)

    # Copy Haude factors
    file.copy(file.path(PELMO_base, "FOCUS", "HAUDE.DAT"), file.path(run_dir, "Haude.dat"))

    # Copy scenario file
    sze_file_upper <- paste0(location_code, "_",
                             toupper(FOCUS_PELMO_crop_sze_names[crop]), ".sze")
    sze_file_lower <- paste0(location_code, "_",
                             FOCUS_PELMO_crop_sze_names[crop], ".sze")
    if (file.exists(file.path(PELMO_base, "FOCUS", sze_file_lower))) {
      sze_file <- sze_file_lower
    } else {
      if (file.exists(file.path(PELMO_base, "FOCUS", sze_file_upper))) {
        sze_file <- sze_file_upper
      } else {
        stop("Could not find szenario file for ", crop, " in ", scenario)
      }
    }
    file.copy(file.path(PELMO_base, "FOCUS", sze_file), file.path(run_dir, sze_file_lower))

    # Generate PELMO input file
    input_file <- file(file.path(run_dir, "pelmo.inp"), encoding = "latin1", open = "w+")
    on.exit(close(input_file))

    add <- function(x) cat(paste0(x, "\r\n"), file = input_file, append = TRUE)

    # How many years do we calculate (26, 46 or 66)?
    psm <- readLines(psm_file, encoding = "latin1")
    number_of_apps_lines <- grep("number of application location", psm)
    absolute_apps_line <- grep(location_code,
                               psm[number_of_apps_lines])
    period <- 1 # application every year
    if (length(absolute_apps_line) == 1) {
      apps_root <- number_of_apps_lines[absolute_apps_line]
    } else {
      apps_root <- number_of_apps_lines[1]
    }

    number_of_apps <- as.integer(substr(psm[apps_root], 1, 3))
    last_app_line <- psm[apps_root + number_of_apps]
    last_app_year <- as.integer(gsub("^.{2,3}  ..  (..)   .*", "\\1",
                                     last_app_line))
    if (last_app_year > 26) period <- 2
    if (last_app_year > 46) period <- 3

    n_years <- switch(as.character(period),
      "1" = 26,
      "2" = 46,
      "3" = 66)

    # First line with number of climate years
    add(paste0(formatC(n_years, width = 3, flag = "0"),
               " 01 01 31 12 Version ", substr(version, 1, 1)))

    # Second line with scenario file (why did we copy it, if this line refers to the FOCUS dir...
    add(paste0("..\\..\\..\\", sze_file_lower))

    # Third line with psm file
    add(basename(psm_file))

    # Lines with climate files
    for (year in 1:n_years) {
      add(paste0("..\\..\\..\\", location_code, "_", formatC(year, width = 2, flag = "0"), ".cli"))
    }

    # Output control section
    add("00000015 00.")
    add("    PRSN    TSER  000.0     1.0 00000001")
    add("    TETD    TSER  000.0     1.0 00000001")
    add("    INFL    TSER  100.0     1.0 00000001")
    add("    RUNF    TSER  000.0     1.0 00000001")
    add("    THET    TSER  000.0     1.0 00000001")
    add("    THET    TSER  030.0     1.0 00000001")
    add("    TEMP    TSER  000.0     1.0 00000001")
    add("    TEMP    TSER  030.0     1.0 00000001")
    add("    TPAP    TSER  000.0  1.0E05 00000001")
    add("    TDKF    TSER  000.0  1.0E05 00000001")
    add("    TUPF    TSER  000.0  1.0E05 00000001")
    add("    TPST    TSER  005.0  1.0E06 00000001")
    add("    PFLX    TSER  100.0  1.0E05 00000001")
    add("    RFLX    TSER  000.0  1.0E05 00000001")
    add("    LEAC    TSER  100.0  1.0E09 00000001")

    # Copy pelmo executable
    file.copy(PELMO_exe, run_dir)

    # In addition to the files copied by the FOCUS PELMO GUI, we
    # need the error file in the run directory, as we start
    # the exe file from this directory
    file.copy(file.path(PELMO_base, "lf90.eer"), run_dir)
  }

  lapply(run_list, setup_run)

  if (execute) {
    run_PELMO(runs, version = version, PELMO_base = PELMO_base)
  }
}

#' Run PELMO
#'
#' @inheritParams PELMO_runs
#' @importFrom parallel mclapply
#' @export
run_PELMO <- function(runs, psm_dir = ".", version = "5.5.3", PELMO_base = "auto",
                      cores = getOption("mc.cores", 2L))
{

  if (PELMO_base[1] == "auto") {
    PELMO_base = file.path(system.file(package = "PELMO.installeR"),
                                       paste0("FOCUSPELMO.", gsub("\\.", "", version)))
  }

  pelmo_exe = list.files(PELMO_base, "^pelmo.*.exe")

  # Create list of runs to traverse using mclapply
  run_list <- create_run_list(runs, check_psm_files = FALSE)

  # In order to run in parallel, we need to make a directory tree starting
  # from the base of the PELMO installation, as pelmo401.exe seems to look
  # for ..\..\..\summary.PLM and does not start if this is in use by another
  # instance
  run_exe <- function(x) {
    psm <- x$psm
    crop <- x$crop
    scenario <- x$scenario
    run_dir <- file.path(PELMO_base, "FOCUS", PELMO_path(psm, crop, scenario))
    exe_dir <- file.path(PELMO_base, paste(sample(c(0:9, letters, LETTERS),
                                                  size = 8, replace = TRUE),
                                           collapse = ""))
    dir.create(exe_dir)

    # Fresh PELMO base directory for every run with random name
    run_dir_exe <- file.path(exe_dir, "FOCUS", PELMO_path(psm, crop, scenario))
    dir.create(run_dir_exe, recursive = TRUE)

    # Copy the contents of the run directory
    run_files <- list.files(run_dir, full.names = TRUE)
    file.copy(run_files, run_dir_exe)

    # Copy FOCUS files
    location_code <- FOCUS_PELMO_location_codes[x$scenario]
    location_files <- list.files(file.path(PELMO_base, "FOCUS"), paste0("^", location_code),
                                 full.names = TRUE)
    focus_dir_exe <- file.path(exe_dir, "FOCUS")
    file.copy(location_files, focus_dir_exe)
    file.copy(file.path(PELMO_base, "FOCUS", "FOCUSCHECK.DAT"), focus_dir_exe)

    # We need to go the directory to simplify calling pelmo with wine
    setwd(run_dir_exe)
    psm_file <- file.path(psm_dir, paste0(psm, ".psm"))
    system(paste("wine", pelmo_exe, psm_file), ignore.stdout = TRUE)

    # Copy the results to the original run directory
    plm_files <- list.files(run_dir_exe, ".plm$", full.names = TRUE)
    PLM_files <- list.files(run_dir_exe, ".PLM$", full.names = TRUE)
    file.copy(c(plm_files, PLM_files), run_dir)

    # Clean up
    unlink(exe_dir, recursive = TRUE)
  }

  mclapply(run_list, run_exe, mc.cores = cores)
}

#' Create a path of run directories as the PELMO GUI does
#'
#' @export
#' @importFrom utils data
#' @param psm The psm identifier
#' @param crop The PELMO crop acronym
#' @param scenario The scenario
PELMO_path <- function(psm, crop, scenario) {
  if (crop %in% names(FOCUS_PELMO_crops)) {
    crop <- FOCUS_PELMO_crops[crop]
  }
  if (!crop %in% FOCUS_PELMO_crops) stop(crop, " is not in FOCUS_PELMO_crops.")
  psm_dir <- paste0(psm, ".run")
  crop_dir <- paste0(gsub(" ", "_-_", crop), ".run")

  # Deal with 'irrigation'. It only affects the naming of the scenario directory,
  # but it does not appear to affect the PELMO run. This can be seen when
  # comparing runs set up for Beans (field) and Beans (vegetable) for the Porto
  # scenario
  irrigation_string <- ""

  # 'Irrigation' is only possible in the GUI for five scenarios
  if (scenario %in% c("Cha", "Pia", "Por", "Sev", "Thi")) {
    crop_acronyms <- names(FOCUS_PELMO_crops)
    names(crop_acronyms) <- FOCUS_PELMO_crops
    crop_acronym <- crop_acronyms[[crop]]
    # Some crops are not 'irrigated' according to the GUI
    if (crop_acronym %in% c("win", "fbe", "woi", "ape", "spr")) {
      irrigation_string <- "--_-_no_-_irrigation"
    } else {
      irrigation_string <- "--_-_irrigated"
    }
  }

  scenario_dir <- paste0(
    FOCUS_GW_scenarios_2012$names[scenario], "_-_(", FOCUS_PELMO_location_codes[scenario], ")",
    irrigation_string, ".run")

  return(file.path(psm_dir, crop_dir, scenario_dir))
}

#' Create a list of runs that we can traverse
#'
#' @inheritParams PELMO_runs
#' @param check_psm_files Should we check if the psm file exists
create_run_list <- function(runs, psm_dir = ".", check_psm_files = FALSE) {
  i <- 0
  run_list <- list()
  for (run in runs) {
    psm <- run$psm
    if (check_psm_files) {
      psm_file <- file.path(psm_dir, paste0(psm, ".psm"))
      if (file.access(psm_file) == -1) {
        stop(psm_file, " is not readable")
      }
    }
    crops <- setdiff(names(run), "psm")
    for (crop in crops) {
      crop_acronyms <- names(FOCUS_PELMO_crops)
      names(crop_acronyms) <- FOCUS_PELMO_crops
      if (!crop %in% crop_acronyms) {
        if (crop %in% FOCUS_PELMO_crops) {
          crop <- crop_acronyms[crop]
        } else {
          stop("Invalid crop specification ", crop)
        }
      }
      for (scenario in run[[crop]]) {
        i <- i + 1
        run_list[[i]] <- list(psm = psm, crop = crop, scenario = scenario)
      }
    }
  }
  return(run_list)
}
