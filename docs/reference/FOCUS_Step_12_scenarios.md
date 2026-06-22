# Step 1/2 scenario data as distributed with the FOCUS Step 1/2 calculator

The data were extracted from the scenario.txt file using the R code
shown below. The text file is not included in the package as its licence
is not clear.

## Format

A list containing the scenario names in a character vector called
'names', the drift percentiles in a matrix called 'drift', interception
percentages in a matrix called 'interception' and the runoff/drainage
percentages for Step 2 calculations in a matrix called 'rd'.

## Examples

``` r

# \dontrun{
  # This is the code that was used to extract the data
  scenario_path <- "inst/extdata/FOCUS_Step_12_scenarios.txt"
  scenarios <- readLines(scenario_path)[9:38]
#> Warning: cannot open file 'inst/extdata/FOCUS_Step_12_scenarios.txt': No such file or directory
#> Error in file(con, "r"): cannot open the connection
  FOCUS_Step_12_scenarios <- list()
  sce <- read.table(text = scenarios, sep = "\t", header = TRUE, check.names = FALSE,
    stringsAsFactors = FALSE)
#> Error: object 'scenarios' not found
  FOCUS_Step_12_scenarios$names = sce$Crop
#> Error: object 'sce' not found
  rownames(sce) <- sce$Crop
#> Error: object 'sce' not found
  FOCUS_Step_12_scenarios$drift = sce[, 3:11]
#> Error: object 'sce' not found
  FOCUS_Step_12_scenarios$interception = sce[, 12:15]
#> Error: object 'sce' not found
  sce_2 <- readLines(scenario_path)[41:46]
#> Warning: cannot open file 'inst/extdata/FOCUS_Step_12_scenarios.txt': No such file or directory
#> Error in file(con, "r"): cannot open the connection
  rd <- read.table(text = sce_2, sep = "\t")[1:2]
#> Error: object 'sce_2' not found
  rd_mat <- matrix(rd$V2, nrow = 3, byrow = FALSE)
#> Error: object 'rd' not found
  dimnames(rd_mat) = list(Time = c("Oct-Feb", "Mar-May", "Jun-Sep"),
                          Region = c("North", "South"))
#> Error: object 'rd_mat' not found
  FOCUS_Step_12_scenarios$rd = rd_mat
#> Error: object 'rd_mat' not found
  save(FOCUS_Step_12_scenarios, file = "data/FOCUS_Step_12_scenarios.RData")
#> Warning: cannot open compressed file 'data/FOCUS_Step_12_scenarios.RData', probable reason 'No such file or directory'
#> Error in gzfile(file, "wb"): cannot open the connection
# }

# And this is the resulting data
FOCUS_Step_12_scenarios
#> list()
```
