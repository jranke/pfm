library(here)

bbch <- paste0(0:9, "x")
crops <- c(
  "Beans (field + vegetable)",
  "Peas",
  "Summer oilseed rape", "Winter oilseed rape",
  "Tomatoes",
  "Spring cereals", "Winter cereals")
EFSA_washoff_2017 <- matrix(NA, length(crops), length(bbch),
  dimnames = list(Crop = crops, BBCH = bbch))
EFSA_washoff_2017["Beans (field + vegetable)", ] <-
  c(NA, 0.6, rep(0.75, 2), rep(0.8, 5), 0.35)
EFSA_washoff_2017["Peas", ] <-
  c(NA, 0.4, rep(0.6, 2), rep(0.65, 5), 0.35)
EFSA_washoff_2017["Summer oilseed rape", ] <-
  c(NA, 0.4, rep(0.5, 2), rep(0.6, 5), 0.5)
EFSA_washoff_2017["Winter oilseed rape", ] <-
  c(NA, 0.1, rep(0.4, 2), rep(0.55, 5), 0.3)
EFSA_washoff_2017["Tomatoes", ] <-
  c(NA, 0.55, rep(0.75, 2), rep(0.7, 5), 0.35)
EFSA_washoff_2017["Spring cereals", ] <-
  c(NA, 0.4, 0.5, 0.5, rep(0.65, 3), rep(0.65, 2), 0.55)
EFSA_washoff_2017["Winter cereals", ] <-
  c(NA, 0.1, 0.4, 0.6, rep(0.55, 3), rep(0.6, 2), 0.4)

save(EFSA_washoff_2017,
  file = here("data/EFSA_washoff_2017.RData"))

