# Retrieve endpoint information from the chyaml field of a chent object

R6 class objects of class chent represent chemical entities and can hold
a list of information loaded from a chemical yaml file in their chyaml
field. Such information is extracted and optionally aggregated by this
function.

## Usage

``` r
endpoint(
  chent,
  medium = "soil",
  type = c("degradation", "sorption"),
  lab_field = c(NA, "laboratory", "field"),
  redox = c(NA, "aerobic", "anaerobic"),
  value = c("DT50ref", "Kfoc", "N"),
  aggregator = geomean,
  raw = FALSE,
  signif = 3
)

soil_DT50(
  chent,
  aggregator = geomean,
  signif = 3,
  lab_field = "laboratory",
  value = "DT50ref",
  redox = "aerobic",
  raw = FALSE
)

soil_Kfoc(chent, aggregator = geomean, signif = 3, value = "Kfoc", raw = FALSE)

soil_N(chent, aggregator = mean, signif = 3, raw = FALSE)

soil_sorption(
  chent,
  values = c("Kfoc", "N"),
  aggregators = c(Kfoc = geomean, Koc = geomean, N = mean),
  signif = c(Kfoc = 3, N = 3),
  raw = FALSE
)
```

## Arguments

- chent:

  The chent object to get the information from

- medium:

  The medium for which information is sought

- type:

  The information type

- lab_field:

  If not NA, do we want laboratory or field endpoints

- redox:

  If not NA, are we looking for aerobic or anaerobic data

- value:

  The name of the value we want. The list given in the usage section is
  not exclusive

- aggregator:

  The aggregator function. Can be mean,
  [`geomean`](https://pkgdown.jrwb.de/pfm/reference/geomean.md), or
  identity, for example.

- raw:

  Should the number(s) be returned as stored in the chent object (could
  be a character value) to retain original information about precision?

- signif:

  How many significant digits do we want

- values:

  The values to be returned

- aggregators:

  A named vector of aggregator functions to be used

## Value

The result from applying the aggregator function to the values converted
to a numeric vector, rounded to the given number of significant digits,
or, if raw = TRUE, the values as a character value, retaining any
implicit information on precision that may be present.

## Details

The functions `soil_*` are functions to extract soil specific endpoints.
For the Freundlich exponent, the capital letter `N` is used in order to
facilitate dealing with such data in R. In pesticide fate modelling,
this exponent is often called 1/n.
