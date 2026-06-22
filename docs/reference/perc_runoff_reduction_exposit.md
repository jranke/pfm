# Runoff reduction percentages as used in Exposit

A table of the runoff reduction percentages used in Exposit 3 for
different vegetated buffer widths

## Usage

``` r
perc_runoff_reduction_exposit
```

## Format

A named list of data frames with reduction percentage values for the
dissolved fraction and the fraction bound to eroding particles, with
vegetated buffer widths as row names. The names of the list items are
the Exposit versions from which the values were taken.

- dissolved:

  The reduction percentage for the dissolved phase

- bound:

  The reduction percentage for the particulate phase

## Source

Excel 3.02 spreadsheet available from
<https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html>

Agroscope version 3.01a with additional runoff factors for 3 m and 6 m
buffer zones received from Muris Korkaric (not published). The variant
3.01a2 was introduced for consistency with previous calculations
performed by Agroscope for a 3 m buffer zone.

## Examples

``` r
print(perc_runoff_reduction_exposit)
#> $`3.02`
#>           dissolved bound
#> No buffer         0     0
#> 5 m              40    40
#> 10 m             60    85
#> 20 m             80    95
#> 
#> $`3.01a`
#>           dissolved bound
#> No buffer         0     0
#> 3 m              25    30
#> 5 m              40    40
#> 6 m              45    55
#> 10 m             60    85
#> 20 m             80    95
#> 
#> $`3.01a2`
#>           dissolved bound
#> No buffer         0     0
#> 3 m              25    25
#> 
#> $`2.0`
#>           dissolved bound
#> No buffer       0.0   0.0
#> 20 m           97.5  97.5
#> 
```
