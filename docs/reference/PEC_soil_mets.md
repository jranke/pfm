# Calculate initial and accumulation PEC soil for a set of metabolites

Calculate initial and accumulation PEC soil for a set of metabolites

## Usage

``` r
PEC_soil_mets(rate, mw_parent, mets, interval = 365, ...)
```

## Arguments

- rate:

  Application rate in units specified below

- mw_parent:

  The molecular weight of the parent compound

- mets:

  A dataframe with metabolite identifiers as rownames and columns "mw",
  "occ" and "DT50" holding their molecular weight, maximum occurrence in
  soil and their soil DT50

- interval:

  The interval for accumulation calculations

- ...:

  Further arguments are passed to PEC_soil
