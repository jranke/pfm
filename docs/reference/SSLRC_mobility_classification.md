# Determine the SSLRC mobility classification for a chemical substance from its Koc

This implements the method specified in the UK data requirements
handbook and was checked against the spreadsheet published on the CRD
website

## Usage

``` r
SSLRC_mobility_classification(Koc)
```

## Arguments

- Koc:

  The sorption coefficient normalised to organic carbon in L/kg

## Value

A list containing the classification and the percentage of the compound
transported per 10 mm drain water

## References

HSE's Chemicals Regulation Division (CRD) Active substance PECsw
calculations (for UK specific authorisation requests)
<https://www.hse.gov.uk/pesticides/topics/pesticide-approvals/pesticides-registration/data-requirements-handbook/fate/active-substance-uk.htm>
accessed 2019-09-27

Drainage PECs Version 1.0 (2015) Spreadsheet published at
<https://www.hse.gov.uk/pesticides/topics/pesticide-approvals/pesticides-registration/data-requirements-handbook/fate/pec-tools-2015/PEC%20sw-sed%20(drainage).xlsx>
accessed 2019-09-27

## Author

Johannes Ranke

## Examples

``` r
SSLRC_mobility_classification(100)
#> $`Mobility classification`
#> [1] "Moderately mobile"
#> 
#> $`Percentage drained per mm of drain water`
#> [1] 0.7
#> 
SSLRC_mobility_classification(10000)
#> $`Mobility classification`
#> [1] "Non mobile"
#> 
#> $`Percentage drained per mm of drain water`
#> [1] 0.008
#> 
```
