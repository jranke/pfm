# Runoff loss percentages as used in Exposit 3

A table of the loss percentages used in Exposit 3 for the twelve
different Koc classes

## Usage

``` r
perc_runoff_exposit
```

## Format

A data frame with percentage values for the dissolved fraction and the
fraction bound to eroding particles, with Koc classes used as row names

- Koc_lower_bound:

  The lower bound of the Koc class

- dissolved:

  The percentage of the applied substance transferred to an adjacent
  water body in the dissolved phase

- bound:

  The percentage of the applied substance transferred to an adjacent
  water body bound to eroding particles

## Source

Excel 3.02 spreadsheet available from
<https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html>

## Examples

``` r
print(perc_runoff_exposit)
#>              Koc_lower_bound dissolved bound
#> 0-20                0 [L/kg]     0.110 0.000
#> >20-50             20 [L/kg]     0.151 0.000
#> >50-100            50 [L/kg]     0.197 0.000
#> >100-200          100 [L/kg]     0.248 0.001
#> >200-500          200 [L/kg]     0.224 0.004
#> >500-1000         500 [L/kg]     0.184 0.020
#> >1000-2000       1000 [L/kg]     0.133 0.042
#> >2000-5000       2000 [L/kg]     0.084 0.091
#> >5000-10000      5000 [L/kg]     0.037 0.159
#> >10000-20000    10000 [L/kg]     0.031 0.192
#> >20000-50000    20000 [L/kg]     0.014 0.291
#> >50000          50000 [L/kg]     0.001 0.451
```
