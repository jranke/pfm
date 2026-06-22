# Regression parameters for the Rautmann drift data

The parameters were extracted from Appendix B to the FOCUS surface water
guidance using the R code given in the file
`data_generation/drift_parameters_focus.R` installed with this package.
The appendix itself is not included in the package, as its licence is
not clear.

## Usage

``` r
drift_parameters_focus
```

## Format

A [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Details

For the hinge distance, `Inf` was substituted for the cases where no
hinge distance is given in the data, in this way parameters C and D are
never used for any distance if A and B are used for the case that the
distance is smaller than the hinge distance.

## References

FOCUS (2014) Generic guidance for Surface Water Scenarios (version 1.4).
FOrum for the Co-ordination of pesticde fate models and their USe.
<http://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/Generic%20FOCUS_SWS_vc1.4.pdf>

FOCUS (2001) FOCUS Surface Water Scenarios in the EU Evaluation Process
under 91/414/EEC. Report of the FOCUS Working Group on Surface Water
Scenarios, EC Document Reference SANCO/4802/2001-rev.2. 245, Appendix B.
<https://esdac.jrc.ec.europa.eu/public_path/projects_data/focus/sw/docs/FOCUS_SWS_APPENDIX_B.doc>

Rautmann, D., Streloke, M and Winkler, R (2001) New basic drift values
in the authorization procedure for plant protection products Mitt. Biol.
Bundesanst. Land- Forstwirtsch. 383, 133-141

## See also

[drift_percentages_rautmann](https://pkgdown.jrwb.de/pfm/reference/drift_percentages_rautmann.md),
[PEC_sw_drift](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_drift.md)

## Examples

``` r
drift_parameters_focus
#> # A tibble: 49 × 8
#>    crop_group n_apps percentile     A      B     C     D hinge
#>    <chr>       <int>      <int> <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1 arable          1         90  2.76 -0.978   NA  NA    Inf  
#>  2 arable          2         82  2.44 -1.01    NA  NA    Inf  
#>  3 arable          3         77  2.02 -0.996   NA  NA    Inf  
#>  4 arable          4         74  1.86 -0.986   NA  NA    Inf  
#>  5 arable          5         72  1.79 -0.994   NA  NA    Inf  
#>  6 arable          6         70  1.63 -0.986   NA  NA    Inf  
#>  7 arable          7         69  1.58 -0.981   NA  NA    Inf  
#>  8 arable          8         67  1.51 -0.983   NA  NA    Inf  
#>  9 hops            1         90 58.2  -1.00  8655. -2.84  15.3
#> 10 hops            2         82 66.2  -1.20  5555. -2.82  15.3
#> # ℹ 39 more rows
unique(drift_parameters_focus$crop_group)
#> [1] "arable"       "hops"         "vines, late"  "vines, early" "fruit, late" 
#> [6] "fruit, early" "aerial"      
```
