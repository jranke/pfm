# Subset of EFSA crop washoff default values

Subset of EFSA crop washoff default values

## Usage

``` r
EFSA_washoff_2017
```

## Format

A matrix containing wash-off factors, currently only for some selected
crops

## Source

European Food Safety Authority (2017) EFSA guidance document for
predicting environmental concentrations of active substances of plant
protection products and transformation products of these active
substances in soil. *EFSA Journal* **15**(10) 4982
doi:10.2903/j.efsa.2017.4982

## Examples

``` r
EFSA_washoff_2017
#>                            BBCH
#> Crop                        0x   1x   2x   3x   4x   5x   6x   7x   8x   9x
#>   Beans (field + vegetable) NA 0.60 0.75 0.75 0.80 0.80 0.80 0.80 0.80 0.35
#>   Peas                      NA 0.40 0.60 0.60 0.65 0.65 0.65 0.65 0.65 0.35
#>   Summer oilseed rape       NA 0.40 0.50 0.50 0.60 0.60 0.60 0.60 0.60 0.50
#>   Winter oilseed rape       NA 0.10 0.40 0.40 0.55 0.55 0.55 0.55 0.55 0.30
#>   Tomatoes                  NA 0.55 0.75 0.75 0.70 0.70 0.70 0.70 0.70 0.35
#>   Spring cereals            NA 0.40 0.50 0.50 0.65 0.65 0.65 0.65 0.65 0.55
#>   Winter cereals            NA 0.10 0.40 0.60 0.55 0.55 0.55 0.60 0.60 0.40
```
