# Calculate predicted environmental concentrations in soil

This is a basic calculation of a contaminant concentration in bulk soil
based on complete, instantaneous mixing. If an interval is given, an
attempt is made at calculating a long term maximum concentration using
the concepts layed out in the PPR panel opinion (EFSA PPR panel 2012 and
in the EFSA guidance on PEC soil calculations (EFSA, 2015, 2017).

## Usage

``` r
PEC_soil(
  rate,
  rate_units = "g/ha",
  interception = 0,
  mixing_depth = 5,
  PEC_units = "mg/kg",
  PEC_pw_units = "mg/L",
  interval = NA,
  n_periods = Inf,
  tillage_depth = 20,
  leaching_depth = tillage_depth,
  crop = "annual",
  cultivation = FALSE,
  chent = NA,
  DT50 = NA,
  FOMC = NA,
  Koc = NA,
  Kom = Koc/1.724,
  t_avg = 0,
  t_act = NULL,
  scenarios = c("default", "EFSA_2017", "EFSA_2015"),
  leaching = scenarios == "EFSA_2017",
  porewater = FALSE
)
```

## Arguments

- rate:

  Application rate in units specified below

- rate_units:

  Defaults to g/ha

- interception:

  The fraction of the application rate that does not reach the soil

- mixing_depth:

  Mixing depth in cm

- PEC_units:

  Requested units for the calculated PEC. Only mg/kg currently supported

- PEC_pw_units:

  Only mg/L currently supported

- interval:

  Period of the deeper mixing. The default is NA, i.e. no deeper mixing.
  For annual deeper mixing, set this to 365 when degradation units are
  in days

- n_periods:

  Number of periods to be considered for long term PEC calculations

- tillage_depth:

  Periodic (see interval) deeper mixing in cm

- leaching_depth:

  EFSA (2017) uses the mixing depth (ecotoxicological evaluation depth)
  to calculate leaching for annual crops where tillage takes place. By
  default, losses from the layer down to the tillage depth are taken
  into account in this implementation.

- crop:

  Ignored for scenarios other than EFSA_2017. Only annual crops are
  supported when these scenarios are used. Only crops with a single
  cropping cycle per year are currently supported.

- cultivation:

  Does mechanical cultivation in the sense of EFSA (2017) take place,
  i.e. twice a year to a depth of 5 cm? Ignored for scenarios other than
  EFSA_2017

- chent:

  An optional chent object holding substance specific information. Can
  also be a name for the substance as a character string

- DT50:

  If specified, overrides soil DT50 endpoints from a chent object If
  DT50 is not specified here and not available from the chent object,
  zero degradation is assumed

- FOMC:

  If specified, it should be a named numeric vector containing the FOMC
  parameters alpha and beta. This overrides any other degradation
  endpoints, and the degradation during the interval and after the
  maximum PEC is calculated using these parameters without temperature
  correction

- Koc:

  If specified, overrides Koc endpoints from a chent object

- Kom:

  Calculated from Koc by default, but can explicitly be specified as Kom
  here

- t_avg:

  Averaging times for time weighted average concentrations

- t_act:

  Time series for actual concentrations

- scenarios:

  If this is 'default', a soil bulk density of 1.5 kg/L will be used.
  The DT50 will be used without correction and soil properties as
  specified in the REACH guidance (R.16, Table R.16-9) are used for
  porewater PEC calculations. If this is "EFSA_2015", the DT50 is taken
  to be a modelling half-life at 20°C and pF2 (for when 'chent' is
  specified, the DegT50 with destination 'PECgw' will be used), and
  corrected using an Arrhenius activation energy of 65.4 kJ/mol. Also
  model and scenario adjustment factors from the EFSA guidance are used.

- leaching:

  Should leaching be taken into account? The default is FALSE, except
  when the EFSA_2017 scenarios are used.

- porewater:

  Should equilibrium porewater concentrations be estimated based on Kom
  and the organic carbon fraction of the soil instead of total soil
  concentrations? Based on equation (7) given in the PPR panel opinion
  (EFSA 2012, p. 24) and the scenarios specified in the EFSA guidance
  (2015, p. 13).

## Value

The predicted concentration in soil

## Details

This assumes that the complete load to soil during the time specified by
'interval' (typically 365 days) is dosed at once. As in the PPR panel
opinion cited below (EFSA PPR panel 2012), only temperature correction
using the Arrhenius equation is performed.

Total soil and porewater PEC values for the scenarios as defined in the
EFSA guidance (2017, p. 14/15) can easily be calculated.

## Note

While time weighted average (TWA) concentrations given in the examples
from the EFSA guidance from 2015 (p. 80) can be reproduced, this is not
true for the TWA concentrations given for the same example in the EFSA
guidance from 2017 (p. 92).

According to the EFSA guidance (EFSA, 2017, p. 43), leaching should be
taken into account for the EFSA 2017 scenarios, using the evaluation
depth (here mixing depth) as the depth of the layer from which leaching
takes place. However, as the amount leaching below the evaluation depth
(often 5 cm) will partly be mixed back during tillage, the default in
this function is to use the tillage depth for the calculation of the
leaching rate.

If temperature information is available in the selected scenarios, as
e.g. in the EFSA scenarios, the DT50 for groundwater modelling
(destination 'PECgw') is taken from the chent object, otherwise the DT50
with destination 'PECsoil'.

## References

EFSA Panel on Plant Protection Products and their Residues (2012)
Scientific Opinion on the science behind the guidance for scenario
selection and scenario parameterisation for predicting environmental
concentrations of plant protection products in soil. *EFSA Journal*
**10**(2) 2562, doi:10.2903/j.efsa.2012.2562

EFSA (European Food Safety Authority) 2017) EFSA guidance document for
predicting environmental concentrations of active substances of plant
protection products and transformation products of these active
substances in soil. *EFSA Journal* **15**(10) 4982
doi:10.2903/j.efsa.2017.4982

EFSA (European Food Safety Authority) (2015) EFSA guidance document for
predicting environmental concentrations of active substances of plant
protection products and transformation products of these active
substances in soil. *EFSA Journal* **13**(4) 4093
doi:10.2903/j.efsa.2015.4093

## Author

Johannes Ranke

## Examples

``` r
PEC_soil(100, interception = 0.25)
#>      scenario
#> t_avg default
#>     0     0.1

# This is example 1 starting at p. 92 of the EFSA guidance (2017)
# Note that TWA concentrations differ from the ones given in the guidance
# for an unknown reason (the values from EFSA (2015) can be reproduced).
PEC_soil(1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
               Kom = 1000, scenarios = "EFSA_2017")
#>      scenario
#> t_avg      CTN     CTC      CTS
#>    0  19.76834 13.8619 10.53795
#>    21 19.59345 13.7169 10.39882
PEC_soil(1000, interval = 365, DT50 = 250, t_av = c(0, 21),
               Kom = 1000, scenarios = "EFSA_2017", porewater = TRUE)
#>      scenario
#> t_avg       CLN       CLC       CLS
#>    0  0.5541984 0.6779249 0.9816693
#>    21 0.5484576 0.6693125 0.9609119

# This is example 1 starting at p. 79 of the EFSA guidance (2015)
PEC_soil(1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
               scenarios = "EFSA_2015")
#>      scenario
#> t_avg      CTN      CTC      CTS
#>    0  21.96827 11.53750 9.145259
#>    21 21.78517 11.40701 9.017370
PEC_soil(1000, interval = 365, DT50 = 250, t_av = c(0, 21),
               Kom = 1000, scenarios = "EFSA_2015", porewater = TRUE)
#>      scenario
#> t_avg       CLN       CLC       CLS
#>    0  0.7589401 0.6674322 0.9147861
#>    21 0.7506036 0.6590345 0.8987279

# The following is from example 4 starting at p. 85 of the EFSA guidance (2015)
# Metabolite M2
# Calculate total and porewater soil concentrations for tier 1 scenarios
# Relative molar mass is 100/300, formation fraction is 0.7 * 1
results_pfm <- PEC_soil(100/300 * 0.7 * 1 * 1000, interval = 365, DT50 = 250, t_avg = c(0, 21),
                        scenarios = "EFSA_2015")
results_pfm_pw <- PEC_soil(100/300 * 0.7 * 1000, interval = 365, DT50 = 250, t_av = c(0, 21),
                           Kom = 100, scenarios = "EFSA_2015", porewater = TRUE)
```
