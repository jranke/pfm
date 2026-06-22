# Properties of the predefined scenarios from the EFSA guidance from 2017

Properties of the predefined scenarios used at Tier 1, Tier 2A and Tier
3A for the concentration in soil as given in the EFSA guidance (2017, p.
14/15). Also, the scenario and model adjustment factors from p. 16 and
p. 18 are included.

## Usage

``` r
soil_scenario_data_EFSA_2017
```

## Format

A data frame with one row for each scenario. Row names are the scenario
codes, e.g. CTN for the Northern scenario for the total concentration in
soil. Columns are mostly self-explanatory. `rho` is the dry bulk density
of the top soil.

## Source

EFSA (European Food Safety Authority) (2017) EFSA guidance document for
predicting environmental concentrations of active substances of plant
protection products and transformation products of these active
substances in soil. *EFSA Journal* **15**(10) 4982
[doi:10.2903/j.efsa.2017.4982](https://doi.org/10.2903/j.efsa.2017.4982)

## Examples

``` r
soil_scenario_data_EFSA_2017
#>        Zone Country T_arit T_arr Texture  f_om theta_fc   rho f_sce f_mod
#> CTN   North Estonia    5.7   7.6  Coarse 0.220    0.244 0.707   1.4     3
#> CTC Central  Poland    7.4   9.3  Coarse 0.122    0.244 0.934   1.4     3
#> CTS   South  France   10.2  11.7  Medium 0.070    0.349 1.117   1.4     3
#> CLN   North Denmark    8.0   9.2  Medium 0.025    0.349 1.371   1.6     4
#> CLC Central Austria    9.3  11.3  Medium 0.018    0.349 1.432   1.6     4
#> CLS   South   Spain   15.4  16.7  Medium 0.010    0.349 1.521   1.6     4
#>     FOCUS_zone prec
#> CTN    Hamburg  639
#> CTC    Hamburg  617
#> CTS    Hamburg  667
#> CLN    Hamburg  602
#> CLC Châteaudun  589
#> CLS    Sevilla  526

waldo::compare(soil_scenario_data_EFSA_2017, soil_scenario_data_EFSA_2015)
#> `old` is length 12
#> `new` is length 10
#> 
#> `names(old)[8:12]`: "rho" "f_sce" "f_mod" "FOCUS_zone" "prec"
#> `names(new)[8:10]`: "rho" "f_sce" "f_mod"                    
#> 
#> `old$Country`: "Estonia" "Poland"  "France" "Denmark" "Austria"        "Spain"
#> `new$Country`: "Estonia" "Germany" "France" "Denmark" "Czech Republik" "Spain"
#> 
#> `old$T_arit`: 5.70 7.40 10.20 8.00 9.30 15.40
#> `new$T_arit`: 4.70 8.00 11.00 8.20 9.10 12.80
#> 
#> `old$T_arr`: 7.60  9.30 11.70 9.20 11.30 16.70
#> `new$T_arr`: 7.00 10.10 12.30 9.80 11.20 14.70
#> 
#> `old$Texture`: "Coarse" "Coarse" "Medium"      "Medium" "Medium" "Medium"
#> `new$Texture`: "Coarse" "Coarse" "Medium fine" "Medium" "Medium" "Medium"
#> 
#> `old$f_om`: 0.2200 0.1220 0.0700 0.0250 0.0180 0.0100
#> `new$f_om`: 0.1180 0.0860 0.0480 0.0230 0.0180 0.0110
#> 
#> `old$theta_fc`: 0.2440 0.2440 0.3490 0.3490 0.3490 0.3490
#> `new$theta_fc`: 0.2440 0.2440 0.3850 0.3470 0.3470 0.3470
#> 
#> `old$rho`: 0.7070 0.9340 1.1170 1.3710 1.4320 1.5210
#> `new$rho`: 0.9500 1.0500 1.2200 1.3900 1.4300 1.5100
#> 
#> `old$f_sce`: 1.40 1.40 1.40 1.60 1.60 1.60
#> `new$f_sce`: 3.00 2.00 2.00 2.00 1.50 1.50
#> 
#> And 3 more differences ...
```
