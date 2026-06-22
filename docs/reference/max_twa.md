# The maximum time weighted average concentration for a moving window

If you generate your time series using
[`sawtooth`](https://pkgdown.jrwb.de/pfm/reference/sawtooth.md), you
need to make sure that the length of the time series allows for finding
the maximum. It is therefore recommended to check this using
[`plot.one_box`](https://pkgdown.jrwb.de/pfm/reference/plot.one_box.md)
using the window size for the argument `max_twa`.

## Usage

``` r
max_twa(x, window = 21)
```

## Arguments

- x:

  An object of type
  [one_box](https://pkgdown.jrwb.de/pfm/reference/one_box.md)

- window:

  The size of the moving window

## Details

The method working directly on fitted
[mkinfit](https://pkgdown.jrwb.de/mkin/reference/mkinfit.html) objects
uses the equations given in the PEC soil section of the FOCUS guidance
and is restricted SFO, FOMC and DFOP models and to the parent compound

## References

FOCUS (2006) “Guidance Document on Estimating Persistence and
Degradation Kinetics from Environmental Fate Studies on Pesticides in EU
Registration” Report of the FOCUS Work Group on Degradation Kinetics, EC
Document Reference Sanco/10058/2005 version 2.0, 434 pp,
<http://esdac.jrc.ec.europa.eu/projects/degradation-kinetics>

## See also

[`twa`](https://pkgdown.jrwb.de/pfm/reference/twa.md)

## Examples

``` r
pred <- sawtooth(one_box(10),
  applications = data.frame(time = c(0, 7), amount = c(1, 1)))
max_twa(pred)
#> $max
#>    parent 
#> 0.9537545 
#> 
#> $window_start
#> parent 
#>      0 
#> 
#> $window_end
#> parent 
#>     21 
#> 
pred_FOMC <- mkinfit("FOMC", FOCUS_2006_C, quiet = TRUE)
max_twa(pred_FOMC)
#>       21 
#> 18.22124 
```
