# Calculate PEC surface water due to runoff and erosion as in Exposit 3

This is a reimplementation of the calculation described in the Exposit
3.02 spreadsheet file, in the worksheet "Konzept Runoff".

## Usage

``` r
PEC_sw_exposit_runoff(
  rate,
  interception = 0,
  Koc,
  DT50 = set_units(Inf, "d"),
  t_runoff = set_units(3, "days"),
  exposit_reduction_version = c("3.02", "3.01a", "3.01a2", "2.0"),
  V_ditch = set_units(30, "m3"),
  V_event = set_units(100, "m3"),
  dilution = 2
)
```

## Source

Excel 3.02 spreadsheet available from
<https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html>

## Arguments

- rate:

  Application rate in g/ha or with a compatible unit specified with the
  units package

- interception:

  The fraction intercepted by the crop

- Koc:

  The sorption coefficient to soil organic carbon

- DT50:

  The soil half-life in days

- t_runoff:

  The time between application and the runoff event, where degradation
  occurs, in days

- exposit_reduction_version:

  The version of the reduction factors to be used. "3.02" is the current
  version used in Germany, "3.01a" is the version with additional
  percentages for 3 m and 6 m buffer zones used in Switzerland. "3.01a2"
  is a version introduced for consistency with previous calculations
  performed for a 3 m buffer zone in Switzerland, with the same
  reduction being applied to the dissolved and the bound fraction.

- V_ditch:

  The volume of the ditch is assumed to be 1 m \* 100 m \* 30 cm = 30 m3

- V_event:

  The unreduced runoff volume, equivalent to 10 mm precipitation on 1 ha

- dilution:

  The dilution factor

## Value

A list containing the following components

- perc_runoff:

  The runoff percentages for dissolved and bound substance

- runoff:

  A matrix containing dissolved and bound input for the different
  distances

- PEC_sw_runoff:

  A dataframe containing PEC values for dissolved and bound substance
  for the different distances. If the rate was given in g/ha, the PECsw
  are in microg/L.

## Details

It is recommened to specify the arguments `rate`, `Koc`, `DT50`,
`t_runoff`, `V_ditch` and `V_event` using
[units::units](https://r-quantities.github.io/units/reference/units.html)
from the `units` package.

## See also

[`perc_runoff_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_exposit.md)
for runoff loss percentages and
[`perc_runoff_reduction_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_reduction_exposit.md)
for runoff reduction percentages used

## Examples

``` r
  PEC_sw_exposit_runoff(500, Koc = 150)
#> $perc_runoff
#> dissolved     bound 
#>     0.248     0.001 
#> 
#> $runoff
#>           dissolved       bound       total
#> No buffer 1.240 [g] 0.00500 [g] 1.24500 [g]
#> 5 m       0.744 [g] 0.00300 [g] 0.74700 [g]
#> 10 m      0.496 [g] 0.00075 [g] 0.49675 [g]
#> 20 m      0.248 [g] 0.00025 [g] 0.24825 [g]
#> 
#> $PEC_sw_runoff
#>                 dissolved              bound           total
#> No buffer 4.769231 [µg/L] 0.019230769 [µg/L] 4.788462 [µg/L]
#> 5 m       4.133333 [µg/L] 0.016666667 [µg/L] 4.150000 [µg/L]
#> 10 m      3.542857 [µg/L] 0.005357143 [µg/L] 3.548214 [µg/L]
#> 20 m      2.480000 [µg/L] 0.002500000 [µg/L] 2.482500 [µg/L]
#> 
  PEC_sw_exposit_runoff(600, Koc = 10000, DT50 = 195, exposit = "3.01a")
#> $perc_runoff
#> dissolved     bound 
#>     0.037     0.159 
#> 
#> $runoff
#>                dissolved          bound          total
#> No buffer 0.21964521 [g] 0.94388078 [g] 1.16352600 [g]
#> 3 m       0.16473391 [g] 0.66071655 [g] 0.82545046 [g]
#> 5 m       0.13178713 [g] 0.56632847 [g] 0.69811560 [g]
#> 6 m       0.12080487 [g] 0.42474635 [g] 0.54555122 [g]
#> 10 m      0.08785809 [g] 0.14158212 [g] 0.22944020 [g]
#> 20 m      0.04392904 [g] 0.04719404 [g] 0.09112308 [g]
#> 
#> $PEC_sw_runoff
#>                  dissolved            bound            total
#> No buffer 0.8447893 [µg/L] 3.6303107 [µg/L] 4.4751000 [µg/L]
#> 3 m       0.7844472 [µg/L] 3.1462693 [µg/L] 3.9307165 [µg/L]
#> 5 m       0.7321507 [µg/L] 3.1462693 [µg/L] 3.8784200 [µg/L]
#> 6 m       0.7106169 [µg/L] 2.4985080 [µg/L] 3.2091248 [µg/L]
#> 10 m      0.6275578 [µg/L] 1.0113008 [µg/L] 1.6388586 [µg/L]
#> 20 m      0.4392904 [µg/L] 0.4719404 [µg/L] 0.9112308 [µg/L]
#> 
```
