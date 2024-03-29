library(here)

# The following code was in the example code of the help page of the data object up to pfm version 0.6.0
# It was not executed after migrating it to this directory (inst/data_generation), because
# the spreadsheet is not available at the JKI website any more.
library(readxl)
abdrift_path <- here("inst/extdata/Tabelle der Abdrifteckwerte.xls")
JKI_crops <- c("Ackerbau", "Obstbau frueh", "Obstbau spaet", "Weinbau frueh", "Weinbau spaet",
  "Hopfenbau", "Flaechenkulturen > 900 l/ha", "Gleisanlagen")
names(JKI_crops) <- c("Field crops", "Pome/stone fruit, early", "Pome/stone fruit, late",
  "Vines early", "Vines late", "Hops", "Areic cultures > 900 L/ha", "Railroad tracks")
drift_data_JKI <- list()

for (n in 1:8) {
  drift_data_raw <- read_excel(abdrift_path, sheet = n + 1, skip = 2)
  drift_data <- matrix(NA, nrow = 9, ncol = length(JKI_crops))
  dimnames(drift_data) <- list(distance = drift_data_raw[[1]][1:9],
                                          crop = JKI_crops)
  if (n == 1) { # Values for railroad tracks only present for one application
    drift_data[, c(1:3, 5:8)] <- as.matrix(drift_data_raw[c(2:7, 11)][1:9, ])
  } else {
    drift_data[, c(1:3, 5:7)] <- as.matrix(drift_data_raw[c(2:7)][1:9, ])
  }
  drift_data_JKI[[n]] <- drift_data
}

# Manual data entry from the Rautmann paper
drift_data_JKI[[1]]["3", "Ackerbau"] <- 0.95
drift_data_JKI[[1]][, "Weinbau frueh"] <- c(NA, 2.7, 1.18, 0.39, 0.2, 0.13, 0.07, 0.04, 0.03)
drift_data_JKI[[2]]["3", "Ackerbau"] <- 0.79
drift_data_JKI[[2]][, "Weinbau frueh"] <- c(NA, 2.53, 1.09, 0.35, 0.18, 0.11, 0.06, 0.03, 0.02)
drift_data_JKI[[3]]["3", "Ackerbau"] <- 0.68
drift_data_JKI[[3]][, "Weinbau frueh"] <- c(NA, 2.49, 1.04, 0.32, 0.16, 0.10, 0.05, 0.03, 0.02)
drift_data_JKI[[4]]["3", "Ackerbau"] <- 0.62
drift_data_JKI[[4]][, "Weinbau frueh"] <- c(NA, 2.44, 1.02, 0.31, 0.16, 0.10, 0.05, 0.03, 0.02)
drift_data_JKI[[5]]["3", "Ackerbau"] <- 0.59
drift_data_JKI[[5]][, "Weinbau frueh"] <- c(NA, 2.37, 1.00, 0.31, 0.15, 0.09, 0.05, 0.03, 0.02)
drift_data_JKI[[6]]["3", "Ackerbau"] <- 0.56
drift_data_JKI[[6]][, "Weinbau frueh"] <- c(NA, 2.29, 0.97, 0.30, 0.15, 0.09, 0.05, 0.03, 0.02)
drift_data_JKI[[7]]["3", "Ackerbau"] <- 0.55
drift_data_JKI[[7]][, "Weinbau frueh"] <- c(NA, 2.24, 0.94, 0.29, 0.15, 0.09, 0.05, 0.03, 0.02)
drift_data_JKI[[8]]["3", "Ackerbau"] <- 0.52
drift_data_JKI[[8]][, "Weinbau frueh"] <- c(NA, 2.16, 0.91, 0.28, 0.14, 0.09, 0.04, 0.03, 0.02)

# Save the data
save(drift_data_JKI,
  file = here("data/drift_data_JKI.RData"))
