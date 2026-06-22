# Actual and maximum moving window time average concentrations for FOMC kinetics

Actual and maximum moving window time average concentrations for FOMC
kinetics

## Usage

``` r
FOMC_actual_twa(
  alpha = 1.0001,
  beta = 10,
  times = c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100)
)
```

## Source

FOCUS (2014) Generic Guidance for Estimating Persistence and Degradation
Kinetics from Environmental Fate Studies on Pesticides in EU
Registration, Version 1.1, 18 December 2014, p. 251

## Arguments

- alpha:

  Parameter of the FOMC model

- beta:

  Parameter of the FOMC model

- times:

  The output times, and window sizes for time weighted average
  concentrations

## Author

Johannes Ranke

## Examples

``` r
FOMC_actual_twa(alpha = 1.0001, beta = 10)
#>         actual       twa
#> 0   1.00000000       NaN
#> 1   0.90908224 0.9530973
#> 2   0.83331814 0.9115995
#> 4   0.71426168 0.8411664
#> 7   0.58820408 0.7580202
#> 14  0.41663019 0.6253074
#> 21  0.32254415 0.5387324
#> 28  0.26312277 0.4767543
#> 42  0.19227599 0.3925054
#> 50  0.16663681 0.3583198
#> 100 0.09088729 0.2397608
```
