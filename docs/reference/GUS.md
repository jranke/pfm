# Groundwater ubiquity score based on Gustafson (1989)

The groundwater ubiquity score GUS is calculated according to the
following equation

## Usage

``` r
GUS(...)

# S3 method for class 'numeric'
GUS(DT50, Koc, ...)

# S3 method for class 'chent'
GUS(
  chent,
  degradation_value = "DT50ref",
  lab_field = "laboratory",
  redox = "aerobic",
  sorption_value = "Kfoc",
  degradation_aggregator = geomean,
  sorption_aggregator = geomean,
  ...
)

# S3 method for class 'GUS_result'
print(x, ..., digits = 1)
```

## Arguments

- ...:

  Included in the generic to allow for further arguments later.
  Therefore this also had to be added to the specific methods.

- DT50:

  Half-life of the chemical in soil. Should be a field half-life
  according to Gustafson (1989). However, leaching to the sub-soil can
  not completely be excluded in field dissipation experiments and
  Gustafson did not refer to any normalisation procedure, but says the
  field study should be conducted under use conditions.

- Koc:

  The sorption constant normalised to organic carbon. Gustafson does not
  mention the nonlinearity of the sorption constant commonly found and
  usually described by Freundlich sorption, therefore it is unclear at
  which reference concentration the Koc should be observed (and if the
  reference concentration would be in soil or in porewater).

- chent:

  If a chent is given with appropriate information present in its chyaml
  field, this information is used, with defaults specified below.

- degradation_value:

  Which of the available degradation values should be used?

- lab_field:

  Should laboratory or field half-lives be used? This defaults to lab in
  this implementation, in order to avoid double-accounting for mobility.
  If comparability with the original GUS values given by
  Gustafson (1989) is desired, non-normalised first-order field
  half-lives obtained under actual use conditions should be used.

- redox:

  Aerobic or anaerobic degradation data

- sorption_value:

  Which of the available sorption values should be used? Defaults to
  Kfoc as this is what is generally available from the European
  pesticide peer review process. These values generally use a reference
  concentration of 1 mg/L in porewater, that means they would be
  expected to be Koc values at a concentration of 1 mg/L in the water
  phase.

- degradation_aggregator:

  Function for aggregating half-lives

- sorption_aggregator:

  Function for aggregation Koc values

- x:

  An object of class GUS_result to be printed

- digits:

  The number of digits used in the print method

## Value

A list with the DT50 and Koc used as well as the resulting score of
class GUS_result

## Details

\$\$GUS = \log\_{10} DT50\_{soil} (4 - \log\_{10} K\_{oc})\$\$

## References

Gustafson, David I. (1989) Groundwater ubiquity score: a simple method
for assessing pesticide leachability. *Environmental toxicology and
chemistry* **8**(4) 339–57.

## Author

Johannes Ranke
