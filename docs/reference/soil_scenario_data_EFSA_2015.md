# Properties of the predefined scenarios from the EFSA guidance from 2015

Properties of the predefined scenarios used at Tier 1, Tier 2A and Tier
3A for the concentration in soil as given in the EFSA guidance (2015, p.
13/14). Also, the scenario and model adjustment factors from p. 15 and
p. 17 are included.

## Usage

``` r
soil_scenario_data_EFSA_2015
```

## Format

A data frame with one row for each scenario. Row names are the scenario
codes, e.g. CTN for the Northern scenario for the total concentration in
soil. Columns are mostly self-explanatory. `rho` is the dry bulk density
of the top soil.

## Source

EFSA (European Food Safety Authority) (2015) EFSA guidance document for
predicting environmental concentrations of active substances of plant
protection products and transformation products of these active
substances in soil. *EFSA Journal* **13**(4) 4093
[doi:10.2903/j.efsa.2015.4093](https://doi.org/10.2903/j.efsa.2015.4093)

## Examples

``` r
soil_scenario_data_EFSA_2015
#>        Zone        Country T_arit T_arr     Texture  f_om theta_fc  rho f_sce
#> CTN   North        Estonia    4.7   7.0      Coarse 0.118    0.244 0.95   3.0
#> CTC Central        Germany    8.0  10.1      Coarse 0.086    0.244 1.05   2.0
#> CTS   South         France   11.0  12.3 Medium fine 0.048    0.385 1.22   2.0
#> CLN   North        Denmark    8.2   9.8      Medium 0.023    0.347 1.39   2.0
#> CLC Central Czech Republik    9.1  11.2      Medium 0.018    0.347 1.43   1.5
#> CLS   South          Spain   12.8  14.7      Medium 0.011    0.347 1.51   1.5
#>     f_mod
#> CTN     2
#> CTC     2
#> CTS     2
#> CLN     4
#> CLC     4
#> CLS     4
```
