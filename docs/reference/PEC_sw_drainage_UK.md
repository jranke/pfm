# Calculate initial predicted environmental concentrations in surface water due to drainage using the UK method

This implements the method specified in the UK data requirements
handbook and was checked against the spreadsheet published on the CRC
website. Degradation between the end (30 April) and the start (1
October) of the drainage period is taken into account if
`latest_application` is specified and the degradation parameters are
given either as a `soil_DT50` or a `model`.

## Usage

``` r
PEC_sw_drainage_UK(
  rate,
  interception = 0,
  Koc,
  latest_application = NULL,
  soil_DT50 = NULL,
  model = NULL,
  model_parms = NULL
)

drainage_date_UK(application_date)
```

## Arguments

- rate:

  Application rate in g/ha or with a compatible unit specified with the
  units package

- interception:

  The fraction of the application rate that does not reach the soil

- Koc:

  The sorption coefficient normalised to organic carbon in L/kg or a
  unit specified with the units package

- latest_application:

  Latest application date, formatted as e.g. "01 July"

- soil_DT50:

  Soil degradation half-life, if SFO kinetics are to be used, in days or
  a time unit specified with the units package

- model:

  The soil degradation model to be used. Either one of "FOMC", "DFOP",
  "HS", or "IORE", or an mkinmod object

- model_parms:

  A named numeric vector containing the model parameters

- application_date:

  Application date

## Value

The predicted concentration in surface water in µg/L

## References

HSE's Chemicals Regulation Division (CRD) Active substance PECsw
calculations (for UK specific authorisation requests)
<https://www.hse.gov.uk/pesticides/data-requirements-handbook/fate/pecsw-sed-via-drainflow.htm>
accessed 2026-02-13

PECsw/sed spray drift and tier 1 drainflow calculator Version 2.1.1
(2025) Spreadsheet published at
<https://www.hse.gov.uk/pesticides/assets/docs/PEC%20sw-sed%20(spraydrift).xlsx)>
accessed 2026-02-13

## Author

Johannes Ranke

## Examples

``` r
PEC_sw_drainage_UK(150, Koc = 100)
#> 8.076923 [µg/L]
PEC_sw_drainage_UK(60, interception = 0.5, Koc = 550,
  latest_application = "01 July", soil_DT50 = 200)
#> 0.8388303 [µg/L]
drainage_date_UK("2023-07-10")
#> [1] "2023-10-01"
drainage_date_UK("2020-12-01")
#> [1] "2020-12-01"
drainage_date_UK(as.Date("2022-01-15"))
#> [1] "2022-01-15"
```
