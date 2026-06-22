# Actual and maximum moving window time average concentrations for SFO kinetics

Actual and maximum moving window time average concentrations for SFO
kinetics

## Usage

``` r
SFO_actual_twa(DT50 = 1000, times = c(0, 1, 2, 4, 7, 14, 21, 28, 42, 50, 100))
```

## Source

FOCUS (2014) Generic Guidance for Estimating Persistence and Degradation
Kinetics from Environmental Fate Studies on Pesticides in EU
Registration, Version 1.1, 18 December 2014, p. 251

## Arguments

- DT50:

  The half-life.

- times:

  The output times, and window sizes for time weighted average
  concentrations

## Author

Johannes Ranke

## Examples

``` r
SFO_actual_twa(10)
#>           actual       twa
#> 0   1.0000000000       NaN
#> 1   0.9330329915 0.9661297
#> 2   0.8705505633 0.9337803
#> 4   0.7578582833 0.8733416
#> 7   0.6155722067 0.7923030
#> 14  0.3789291416 0.6400113
#> 21  0.2332582479 0.5267498
#> 28  0.1435872944 0.4412651
#> 42  0.0544094102 0.3248093
#> 50  0.0312500000 0.2795222
#> 100 0.0009765625 0.1441286
```
