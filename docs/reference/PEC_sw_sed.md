# Calculate predicted environmental concentrations in sediment from surface water concentrations

The method 'percentage' is equivalent to what is used in the CRD
spreadsheet PEC calculator

## Usage

``` r
PEC_sw_sed(
  PEC_sw,
  percentage = 100,
  method = "percentage",
  sediment_depth = set_units(5, "cm"),
  water_depth = set_units(30, "cm"),
  sediment_density = set_units(1.3, "kg/L"),
  PEC_sed_units = c("µg/kg", "mg/kg")
)
```

## Arguments

- PEC_sw:

  Numeric vector or matrix of surface water concentrations in µg/L for
  which the corresponding sediment concentration is to be estimated

- percentage:

  The percentage in sediment, used for the percentage method

- method:

  The method used for the calculation

- sediment_depth:

  Depth of the sediment layer

- water_depth:

  Depth of the water body in cm

- sediment_density:

  The density of the sediment in kg/L (equivalent to g/cm3)

- PEC_sed_units:

  The units of the estimated sediment PEC value

## Value

The predicted concentration in sediment

## Author

Johannes Ranke

## Examples

``` r
library(pfm)
library(units)
PEC_sw_sed(PEC_sw_drift(100, distances = 1), percentage = 50)
#> 2.130769 [µg/kg]
```
