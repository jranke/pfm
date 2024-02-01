library(here)
library(docxtractr)
library(dplyr)

# The file FOCUS_SWS_APPENDIX_B.doc was downloaded
# from https://esdac.jrc.ec.europa.eu/projects/surface-water
# using URL https://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/FOCUS_SWS_APPENDIX_B.doc
# It was then opened in Microsoft Word (Microsoft Office LTSC Professional Plus 2021)
# and saved as a .docx file in inst/extdata
docx <- read_docx(here("inst/extdata/FOCUS_SWS_APPENDIX_B.docx"))

table_1 <- docx_extract_tbl(docx, 1)
table_2 <- docx_extract_tbl(docx, 2)

combined <- rbind(table_1[-1, ], table_2[-1, ])
names(combined) <- c("crop_group", "n_apps", "percentile", "A", "B", "C", "D", "hinge")
combined$crop_group = c(
  rep(
    c("arable", "hops", "vines, late", "vines, early", "fruit, late", "fruit, early"), 
      each = 8), 
  "aerial")

drift_parameters_focus <- combined |>
  mutate(across(2:3, as.integer)) |>
  mutate(across(4:8, as.numeric)) |>
  mutate(hinge = if_else(is.na(hinge), Inf, hinge))

save(drift_parameters_focus,
  file = here("data/drift_parameters_focus.RData"))

