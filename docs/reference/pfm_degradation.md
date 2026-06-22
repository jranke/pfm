# Calculate a time course of relative concentrations based on an mkinmod model

Calculate a time course of relative concentrations based on an mkinmod
model

## Usage

``` r
pfm_degradation(
  model = "SFO",
  DT50 = 1000,
  parms = c(k_parent = log(2)/DT50),
  years = 1,
  step_days = 1,
  times = seq(0, years * 365, by = step_days)
)
```

## Arguments

- model:

  The degradation model to be used. Either a parent only model like
  'SFO' or 'FOMC', or an mkinmod object

- DT50:

  The half-life. This is only used when simple exponential decline is
  calculated (SFO model).

- parms:

  The parameters used for the degradation model

- years:

  For how many years should the degradation be predicted?

- step_days:

  What step size in days should the output have?

- times:

  The output times

## Author

Johannes Ranke

## Examples

``` r
head(pfm_degradation("SFO", DT50 = 10))
#>   time    parent
#> 0    0 1.0000000
#> 1    1 0.9330330
#> 2    2 0.8705506
#> 3    3 0.8122524
#> 4    4 0.7578583
#> 5    5 0.7071068
```
