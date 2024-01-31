library(here)

# Data from 2015
soil_scenario_data_EFSA_2015 <- data.frame(
  Zone = rep(c("North", "Central", "South"), 2),
  Country = c("Estonia", "Germany", "France", "Denmark", "Czech Republik", "Spain"),
  T_arit = c(4.7, 8.0, 11.0, 8.2, 9.1, 12.8),
  T_arr = c(7.0, 10.1, 12.3, 9.8, 11.2, 14.7),
  Texture = c("Coarse", "Coarse", "Medium fine", "Medium", "Medium", "Medium"),
  f_om = c(0.118, 0.086, 0.048, 0.023, 0.018, 0.011),
  theta_fc = c(0.244, 0.244, 0.385, 0.347, 0.347, 0.347),
  rho = c(0.95, 1.05, 1.22, 1.39, 1.43, 1.51),
  f_sce = c(3, 2, 2, 2, 1.5, 1.5),
  f_mod = c(2, 2, 2, 4, 4, 4),
  stringsAsFactors = FALSE,
  row.names = c("CTN", "CTC", "CTS", "CLN", "CLC", "CLS")
)
save(soil_scenario_data_EFSA_2015,
  file = here('data/soil_scenario_data_EFSA_2015.RData'))

# Data from 2017
soil_scenario_data_EFSA_2017 <- data.frame(
  Zone = rep(c("North", "Central", "South"), 2),
  Country = c("Estonia", "Poland", "France", "Denmark", "Austria", "Spain"),
  T_arit = c(5.7, 7.4, 10.2, 8.0, 9.3, 15.4),
  T_arr = c(7.6, 9.3, 11.7, 9.2, 11.3, 16.7),
  Texture = c("Coarse", "Coarse", "Medium", "Medium", "Medium", "Medium"),
  f_om = c(0.220, 0.122, 0.070, 0.025, 0.018, 0.010),
  theta_fc = c(0.244, 0.244, 0.349, 0.349, 0.349, 0.349),
  rho = c(0.707, 0.934, 1.117, 1.371, 1.432, 1.521),
  f_sce = rep(c(1.4, 1.6), each = 3),
  f_mod = rep(c(3, 4), each = 3),
  FOCUS_zone = c("Hamburg", "Hamburg", "Hamburg", "Hamburg", "Ch\u00e2teaudun", "Sevilla"),
  prec = c(639, 617, 667, 602, 589, 526),
  stringsAsFactors = FALSE,
  row.names = c("CTN", "CTC", "CTS", "CLN", "CLC", "CLS")
)

save(soil_scenario_data_EFSA_2017,
  file = here('data/soil_scenario_data_EFSA_2017.RData'))
