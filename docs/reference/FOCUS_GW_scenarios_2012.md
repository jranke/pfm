# A very small subset of the FOCUS Groundwater scenario definitions

Currently, only scenario names with acronyms and a small subset of the
soil definitions are provided. The soil definitions are from page 46ff.
from FOCUS (2012).

## Usage

``` r
FOCUS_GW_scenarios_2012
```

## References

FOCUS (2012) Generic guidance for Tier 1 FOCUS ground water assessments.
Version 2.1. FOrum for the Co-ordination of pesticde fate models and
their USe.
http://focus.jrc.ec.europa.eu/gw/docs/Generic_guidance_FOCV2_1.pdf

## Examples

``` r
FOCUS_GW_scenarios_2012
#> $names
#>            Cha            Ham            Jok            Kre            Oke 
#>    "Châteadun"      "Hamburg"    "Jokioinen" "Kremsmünster"   "Okehampton" 
#>            Pia            Por            Sev            Thi 
#>     "Piacenza"        "Porto"      "Sevilla"        "Thiva" 
#> 
#> $soils
#>    location horizon number pH_H2O perc_clay perc_oc rel_deg
#> 1       Cha      Ap      1    8.0      30.0    1.39     1.0
#> 2       Cha      B1      2    8.1      31.0    0.93     0.5
#> 3       Cha      B2      3    8.2      25.0    0.70     0.5
#> 4       Cha   II C1      4    8.5      26.0    0.30     0.3
#> 5       Cha   II C1      5    8.5      26.0    0.30     0.0
#> 6       Cha   II C2      6    8.5      24.0    0.27     0.0
#> 7       Cha       M      7    8.3      31.0    0.21     0.0
#> 8       Ham      Ap      1    6.4       7.2    1.50     1.0
#> 9       Ham     BvI      2    5.6       6.7    1.00     0.5
#> 10      Ham    BvII      3    5.6       0.9    0.20     0.3
#> 11      Ham   Bv/Cv      4    5.7       0.0    0.00     0.3
#> 12      Ham      Cv      5    5.5       0.0    0.00     0.3
#> 13      Ham      Cv      6    5.5       0.0    0.00     0.0
#> 14      Jok      Ap      1    6.2       3.6    4.06     1.0
#> 15      Jok      Bs      2    5.6       1.8    0.84     0.5
#> 16      Jok     BC1      3    5.4       1.2    0.36     0.3
#> 17      Jok     BC2      4    5.4       1.7    0.29     0.3
#> 18      Jok     BC2      5    5.4       1.7    0.29     0.0
#> 19      Jok      Cg      6    5.3       1.9    0.21     0.0
#> 20      Kre    <NA>      1    7.7      14.0    3.60     1.0
#> 21      Kre    <NA>      2    7.0      25.0    1.00     0.5
#> 22      Kre    <NA>      3    7.1      27.0    0.50     0.5
#> 23      Kre    <NA>      4    7.1      27.0    0.50     0.3
#> 24      Kre    <NA>      5    7.1      27.0    0.50     0.0
#> 25      Oke       A      1    5.8      18.0    2.20     1.0
#> 26      Oke     Bw1      2    6.3      17.0    0.70     0.5
#> 27      Oke      BC      3    6.5      14.0    0.40     0.3
#> 28      Oke       C      4    6.6       9.0    0.10     0.3
#> 29      Oke       C      5    6.6       9.0    0.10     0.0
#> 30      Pia      Ap      1    7.0      15.0    1.26     1.0
#> 31      Pia      Ap      2    7.0      15.0    1.26     0.5
#> 32      Pia      Bw      3    6.3       7.0    0.47     0.5
#> 33      Pia      Bw      4    6.3       7.0    0.47     0.3
#> 34      Pia      2C      5    6.4       0.0    0.00     0.3
#> 35      Pia      2C      6    6.4       0.0    0.00     0.0
#> 36      Por    <NA>      1    4.9      10.0    1.42     1.0
#> 37      Por    <NA>      2    4.8       8.0    0.78     0.5
#> 38      Por    <NA>      3    4.8       8.0    0.78     0.3
#> 39      Por    <NA>      4    4.8       8.0    0.78     0.0
#> 40      Sev    <NA>      1    7.3      14.0    0.93     1.0
#> 41      Sev    <NA>      2    7.3      13.0    0.93     1.0
#> 42      Sev    <NA>      3    7.8      15.0    0.70     0.5
#> 43      Sev    <NA>      4    8.1      16.0    0.58     0.3
#> 44      Sev    <NA>      5    8.1      16.0    0.58     0.0
#> 45      Sev    <NA>      6    8.2      22.0    0.49     0.0
#> 46      Thi     Ap1      1    7.7      25.3    0.74     1.0
#> 47      Thi     Ap2      2    7.7      25.3    0.74     0.5
#> 48      Thi      Bw      3    7.8      29.6    0.57     0.5
#> 49      Thi      Bw      4    7.8      31.9    0.31     0.3
#> 50      Thi     Ck1      5    7.8      32.9    0.18     0.3
#> 51      Thi     Ck1      6    7.8      32.9    0.18     0.0
#> 
```
