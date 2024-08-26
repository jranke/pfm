library(here)
library(units)
library(tibble)

# Runoff percentages
Koc_breaks <- c(0, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, Inf)
tmp <- paste(Koc_breaks[1:11], Koc_breaks[2:12], sep = "-")
Koc_classes <- c(tmp[1], paste0(">", tmp[2:11]), ">50000")
perc_runoff_exposit <- data.frame(
  Koc_lower_bound = Koc_breaks[1:12],
  dissolved = c(0.11, 0.151, 0.197, 0.248, 0.224, 0.184, 0.133, 0.084, 0.037, 0.031, 0.014, 0.001),
  bound = c(0, 0, 0, 0.001, 0.004, 0.020, 0.042, 0.091, 0.159, 0.192, 0.291, 0.451)) |>
  mutate(Koc_lower_bound = set_units(Koc_lower_bound, "L/kg"))
rownames(perc_runoff_exposit) <- Koc_classes

# Runoff reduction percentages
perc_runoff_reduction_exposit <- list(
   "3.02" = data.frame(
    dissolved = c(0, 40, 60, 80),
    bound = c(0, 40, 85, 95),
    row.names = c("No buffer", paste(c(5, 10, 20), "m"))),
   "3.01a" = data.frame(
    dissolved = c(0, 25, 40, 45, 60, 80),
    bound = c(0, 30, 40, 55, 85, 95),
    row.names = c("No buffer", paste(c(3, 5, 6, 10, 20), "m"))),
   "3.01a2" = data.frame(
    dissolved = c(0, 25),
    bound = c(0, 25),
    row.names = c("No buffer", paste(c(3), "m"))),
  "2.0" = data.frame(
    dissolved = c(0, 97.5),
    bound = c(0, 97.5),
    row.names = c("No buffer", "20 m"))
)

save(perc_runoff_exposit, file = here("data/perc_runoff_exposit.RData"))
save(perc_runoff_reduction_exposit, file = here("data/perc_runoff_reduction_exposit.RData"))
