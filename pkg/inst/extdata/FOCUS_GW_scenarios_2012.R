# FOCUS 2012 p. 46 ff
FOCUS_GW_scenarios_2012 = list()

n_layers = c(7, 6, 6, 5, 5, 6, 4, 6, 6)
acronyms = c("Cha", "Ham", "Jok", "Kre", "Oke", "Pia", "Por", "Sev", "Thi")
FOCUS_GW_scenarios_2012$names = c("Ch\u00e2teadun", "Hamburg", "Jokioinen",
                                  "Kremsm\u00fcnster", "Okehampton",
                                  "Piacenza", "Porto", "Sevilla", "Thiva")

names(FOCUS_GW_scenarios_2012$names) = acronyms
FOCUS_GW_scenarios_2012$soils <- data.frame(
  location= rep(acronyms, times = n_layers),
  horizon = c("Ap", "B1", "B2", "II C1", "II C1", "II C2", "M",
              "Ap", "BvI", "BvII", "Bv/Cv", "Cv", "Cv",
              "Ap", "Bs", "BC1", "BC2", "BC2", "Cg",
              rep(NA, 5),
              "A", "Bw1", "BC", "C", "C",
              "Ap", "Ap", "Bw", "Bw", "2C", "2C",
              rep(NA, 4),
              rep(NA, 6),
              "Ap1", "Ap2", "Bw", "Bw", "Ck1", "Ck1"),
  number = unlist(sapply(n_layers, function(x) 1:x)),
  pH_H2O = c(8.0, 8.1, 8.2, 8.5, 8.5, 8.5, 8.3,
             6.4, 5.6, 5.6, 5.7, 5.5, 5.5,
             6.2, 5.6, 5.4, 5.4, 5.4, 5.3,
             7.7, 7.0, 7.1, 7.1, 7.1,
             5.8, 6.3, 6.5, 6.6, 6.6,
             7, 7, 6.3, 6.3, 6.4, 6.4,
             4.9, 4.8, 4.8, 4.8,
             7.3, 7.3, 7.8, 8.1, 8.1, 8.2,
             7.7, 7.7, 7.8, 7.8, 7.8, 7.8),
  perc_clay = c(30, 31, 25, 26, 26, 24, 31,
                7.2, 6.7, 0.9, 0, 0, 0,
                3.6, 1.8, 1.2, 1.7, 1.7, 1.9,
                14, 25, 27, 27, 27,
                18, 17, 14, 9, 9,
                15, 15, 7, 7, 0, 0,
                10, 8, 8, 8,
                14, 13, 15, 16, 16, 22,
                25.3, 25.3, 29.6, 31.9, 32.9, 32.9),
  perc_oc = c(1.39, 0.93, 0.7, 0.3, 0.3, 0.27, 0.21,
              1.5, 1, 0.2, 0, 0, 0,
              4.06, 0.84, 0.36, 0.29, 0.29, 0.21,
              3.6, 1.0, 0.5, 0.5, 0.5,
              2.2, 0.7, 0.4, 0.1, 0.1,
              1.26, 1.26, 0.47, 0.47, 0, 0,
              1.42, 0.78, 0.78, 0.78,
              0.93, 0.93, 0.70, 0.58, 0.58, 0.49,
              0.74, 0.74, 0.57, 0.31, 0.18, 0.18),
  rel_deg = c(1, 0.5, 0.5, 0.3, 0, 0, 0,
              1, 0.5, 0.3, 0.3, 0.3, 0,
              1, 0.5, 0.3, 0.3, 0, 0,
              1, 0.5, 0.5, 0.3, 0,
              1, 0.5, 0.3, 0.3, 0,
              1, 0.5, 0.5, 0.3, 0.3, 0,
              1, 0.5, 0.3, 0,
              1, 1, 0.5, 0.3, 0, 0,
              1, 0.5, 0.5, 0.3, 0.3, 0))
save(FOCUS_GW_scenarios_2012, file = "../../data/FOCUS_GW_scenarios_2012.RData")
