# Calculate PEC surface water due to drainage as in Exposit 3

This is a reimplementation of the calculation described in the Exposit
3.02 spreadsheet file, in the worksheet "Konzept Drainage". Although
there are four groups of compounds ("Gefährdungsgruppen"), only one
distinction is made in the calculations, between compounds with low
mobility (group 1) and compounds with modest to high mobility (groups 2,
3 and 4). In this implementation, the group is derived only from the
Koc, if not given explicitly. For details, see the discussion of the
function arguments below. It is recommened to specify the arguments
`rate`, `Koc`, `DT50`, `t_drainage`, `V_ditch` and `V_drainage` using
[units::units](https://r-quantities.github.io/units/reference/units.html)
from the `units` package.

## Usage

``` r
PEC_sw_exposit_drainage(
  rate,
  interception = 0,
  Koc = NA,
  mobility = c(NA, "low", "high"),
  DT50 = set_units(Inf, "d"),
  t_drainage = set_units(3, "days"),
  V_ditch = set_units(30, "m3"),
  V_drainage = set_units(c(spring = 10, autumn = 100), "m3"),
  dilution = 2
)
```

## Source

Excel 3.02 spreadsheet available from
<https://www.bvl.bund.de/SharedDocs/Downloads/04_Pflanzenschutzmittel/zul_umwelt_exposit.html>

## Arguments

- rate:

  The application rate in g/ha

- interception:

  The fraction intercepted by the crop

- Koc:

  The sorption coefficient to soil organic carbon used to determine the
  mobility. A trigger value of 550 L/kg is used in order to decide if
  Koc \>\> 500.

- mobility:

  Overrides what is determined from the Koc.

- DT50:

  The soil half-life in days

- t_drainage:

  The time between application and the drainage event, where degradation
  occurs, in days

- V_ditch:

  The volume of the ditch is assumed to be 1 m \* 100 m \* 30 cm = 30 m3

- V_drainage:

  The drainage volume, equivalent to 1 mm precipitation on 1 ha for
  spring/summer or 10 mm for autumn/winter/early spring.

- dilution:

  The dilution factor

## Value

A list containing the following components

- perc_drainage_total:

  Gesamtaustrag (total fraction of the residue drained)

- perc_peak:

  Stoßbelastung (fraction drained at event)

- PEC_sw_drainage:

  A matrix containing PEC values for the spring and autumn scenarios. If
  the rate was given in g/ha, the PECsw are in microg/L.

## See also

[`perc_runoff_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_exposit.md)
for runoff loss percentages and
[`perc_runoff_reduction_exposit`](https://pkgdown.jrwb.de/pfm/reference/perc_runoff_reduction_exposit.md)
for runoff reduction percentages used

## Examples

``` r
  PEC_sw_exposit_drainage(500, Koc = 150)
#> $perc_drainage_total
#> spring autumn 
#>    0.2    1.0 
#> 
#> $perc_peak
#> spring autumn 
#>   12.5   25.0 
#> 
#> $PEC_sw_drainage
#> Units: [µg/L]
#>   spring   autumn 
#> 1.562500 4.807692 
#> 
```
