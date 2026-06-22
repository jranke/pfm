# Estimation of the transpiration stream concentration factor

The FOCUS groundwater guidance (FOCUS 2014, p. 41) states that a
reliable measured log Kow for neutral pH must be available in order to
apply the Briggs equation. It is not clarified when it can be regarded
reliable, but the equation is stated to be produced for non-ionic
compounds, suggesting that the compound should not be ionogenic (weak
acid/base) or ionic.

## Usage

``` r
TSCF(log_Kow, method = c("briggs82", "dettenmaier09"))
```

## Arguments

- log_Kow:

  The decadic logarithm of the octanol-water partition constant

- method:

  Short name of the estimation method.

## Details

The Dettenmaier equation is given to show that other views on the
subject exist.

## References

FOCUS (2014) Generic Guidance for Tier 1 FOCUS Ground Water Assessments.
Version 2.2, May 2014 Dettenmaier EM, Doucette WJ and Bugbee B (2009)
Chemical hydrophobicity and uptake by plant roots. Environ. Sci. Technol
43, 324 - 329

## Examples

``` r
plot(TSCF, -1, 5, xlab = "log Kow", ylab = "TSCF", ylim = c(0, 1.1))
TSCF_2 <- function(x) TSCF(x, method = "dettenmaier09")
curve(TSCF_2, -1, 5, add = TRUE, lty = 2)
legend("topright", lty = 1:2, bty = "n",
  legend = c("Briggs et al. (1982)", "Dettenmaier et al. (2009)"))
```
