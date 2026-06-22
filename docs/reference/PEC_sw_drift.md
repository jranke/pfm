# Calculate predicted environmental concentrations in surface water due to drift

This is a basic, vectorised form of a simple calculation of a
contaminant concentration in surface water based on complete,
instantaneous mixing with input via spray drift.

## Usage

``` r
PEC_sw_drift(
  rate,
  applications = 1,
  water_depth = as_units("30 cm"),
  drift_percentages = NULL,
  drift_data = c("JKI", "RF"),
  crop_group_JKI = "Ackerbau",
  crop_group_RF = "arable",
  distances = c(1, 5, 10, 20),
  formula = c("Rautmann", "FOCUS"),
  water_width = as_units("100 cm"),
  side_angle = 90,
  rate_units = "g/ha",
  PEC_units = "µg/L"
)
```

## Arguments

- rate:

  Application rate in units specified below, or with units defined via
  the `units` package.

- applications:

  Number of applications for selection of drift percentile

- water_depth:

  Depth of the water body in cm

- drift_percentages:

  Percentage drift values for which to calculate PECsw. Overrides
  'drift_data', 'distances', 'applications', crop group and formula
  arguments if not NULL.

- drift_data:

  Source of drift percentage data. If 'JKI', the
  [drift_data_JKI](https://pkgdown.jrwb.de/pfm/reference/drift_data_JKI.md)
  included in the package is used. If 'RF', the Rautmann drift data are
  calculated either in the original form or integrated over the width of
  the water body, depending on the 'formula' argument.

- crop_group_JKI:

  When using the 'JKI' drift data, one of the German names as used in
  [drift_data_JKI](https://pkgdown.jrwb.de/pfm/reference/drift_data_JKI.md).
  Will only be used if drift_data is 'JKI'. Available crop groups are
  "Ackerbau", "Obstbau frueh", "Obstbau spaet", "Weinbau frueh",
  "Weinbau spaet", "Hopfenbau", "Flaechenkulturen \> 900 l/ha" and
  "Gleisanlagen".

- crop_group_RF:

  Crop group(s) as used in
  [drift_parameters_focus](https://pkgdown.jrwb.de/pfm/reference/drift_parameters_focus.md),
  i.e. "arable", "hops", "vines, late", "vines, early", "fruit, late",
  "fruit, early" or "aerial".

- distances:

  The distances in m for which to get PEC values

- formula:

  By default, the original Rautmann formula is used. If you specify
  "FOCUS", mean drift input over the width of the water body is
  calculated as described in Chapter 5.4.5 of the FOCUS surface water
  guidance

- water_width:

  Width of the water body in cm

- side_angle:

  The angle of the side of the water relative to the bottom which is
  assumed to be horizontal, in degrees. The SYNOPS model assumes 45
  degrees here.

- rate_units:

  Defaults to g/ha. For backwards compatibility, only used if the
  specified rate does not have
  [units::units](https://r-quantities.github.io/units/reference/units.html)\].

- PEC_units:

  Requested units for the calculated PEC. Only µg/L currently supported

## Value

A numeric vector with the predicted concentration in surface water. In
some cases, the vector is named with distances or drift percentages, for
backward compatibility with versions before the vectorisation of
arguments other than 'distances' was introduced in v0.6.5.

## Details

It is recommened to specify the arguments `rate`, `water_depth` and
`water_width` using
[units::units](https://r-quantities.github.io/units/reference/units.html)
from the `units` package.

Since pfm version 0.6.5, the function is vectorised with respect to
rates, applications, water depth, crop groups and distances

## See also

[drift_parameters_focus](https://pkgdown.jrwb.de/pfm/reference/drift_parameters_focus.md),
[drift_percentages_rautmann](https://pkgdown.jrwb.de/pfm/reference/drift_percentages_rautmann.md)

## Author

Johannes Ranke

## Examples

``` r
PEC_sw_drift(100)
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.92333333 0.19000000 0.09666667 0.05000000 
# Alternatively, we can use the formula for a single application to
# "Ackerbau" from the paper
PEC_sw_drift(100, drift_data = "RF")
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.91976667 0.19064473 0.09680051 0.04915079 

# This makes it possible to also use different distances
PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF")
#> Units: [µg/L]
#>        1 m        3 m        5 m        6 m       10 m       20 m       50 m 
#> 0.91976667 0.31415827 0.19064473 0.15951494 0.09680051 0.04915079 0.02006434 
#>      100 m 
#> 0.01018774 

# or consider aerial application
PEC_sw_drift(100, distances = c(1, 3, 5, 6, 10, 20, 50, 100), drift_data = "RF",
  crop_group_RF = "aerial")
#> Units: [µg/L]
#>        1 m        3 m        5 m        6 m       10 m       20 m       50 m 
#> 16.8233333 11.0585820  9.0986174  8.4866460  6.9825178  4.7004640  1.8820816 
#>      100 m 
#>  0.9417586 

# Using custom drift percentages is also supported
PEC_sw_drift(100, drift_percentages = c(2.77, 0.95, 0.57, 0.48, 0.29, 0.15, 0.06, 0.03))
#> Units: [µg/L]
#>     2.77 %     0.95 %     0.57 %     0.48 %     0.29 %     0.15 %     0.06 % 
#> 0.92333333 0.31666667 0.19000000 0.16000000 0.09666667 0.05000000 0.02000000 
#>     0.03 % 
#> 0.01000000 

# The influence of assuming a 45° angle of the sides of the waterbody and the width of the
# waterbody can be illustrated
PEC_sw_drift(100)
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.92333333 0.19000000 0.09666667 0.05000000 
PEC_sw_drift(100, drift_data = "RF")
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.91976667 0.19064473 0.09680051 0.04915079 
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS")
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.64246407 0.17414541 0.09235842 0.04798749 
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45)
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.91780582 0.24877916 0.13194060 0.06855356 
PEC_sw_drift(100, drift_data = "RF", formula = "FOCUS", side_angle = 45, water_width = 200)
#> Units: [µg/L]
#>        1 m        5 m       10 m       20 m 
#> 0.60169999 0.18937304 0.10402698 0.05517095 

# The function is vectorised with respect to rates, applications, water depth,
# crop groups and distances
PEC_sw_drift(
  rate = rep(100, 6),
  applications = c(1, 2, rep(1, 4)),
  water_depth = c(30, 30, 30, 60, 30, 30),
  crop_group_JKI = c(rep("Ackerbau", 4), rep("Obstbau frueh", 2)),
  distances = c(rep(5, 4), 10, 5))
#> Units: [µg/L]
#>       5 m       5 m       5 m       5 m      10 m       5 m 
#> 0.1900000 0.1566667 0.1900000 0.0950000 3.9366667 6.6300000 

# Try the same with the Rautmann formula
PEC_sw_drift(
  rate = rep(100, 6),
  applications = c(1, 2, rep(1, 4)),
  water_depth = c(30, 30, 30, 60, 30, 30),
  drift_data = "RF",
  crop_group_RF = c(rep("arable", 4), rep("fruit, early", 2)),
  distances = c(rep(5, 4), 10, 5))
#> Units: [µg/L]
#>        5 m        5 m        5 m        5 m       10 m        5 m 
#> 0.19064473 0.15991216 0.19064473 0.09532236 3.93566026 6.62814740 

# And with the FOCUS variant
PEC_sw_drift(
  rate = rep(100, 6),
  applications = c(1, 2, rep(1, 4)),
  water_depth = c(30, 30, 30, 60, 30, 30),
  drift_data = "RF",
  formula = "FOCUS",
  crop_group_RF = c(rep("arable", 4), rep("fruit, early", 2)),
  distances = c(rep(5, 4), 10, 5))
#> Units: [µg/L]
#>       5 m       5 m       5 m       5 m      10 m       5 m 
#> 0.1741454 0.1456444 0.1741454 0.0870727 3.7957683 6.1809560 
```
