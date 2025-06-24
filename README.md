# pfm

[![Online documentation](https://img.shields.io/badge/docs-jrwb.de-blue.svg)](https://pkgdown.jrwb.de/pfm/)
[![pfm status badge](https://jranke.r-universe.dev/badges/pfm)](https://jranke.r-universe.dev/ui/#package:pfm)
[![Build Status](https://app.travis-ci.com/jranke/pfm.svg?token=Sq9VuYWyRz2FbBLxu6DK&branch=main)](https://app.travis-ci.com/jranke/pfm)
[![codecov](https://codecov.io/github/jranke/pfm/branch/main/graphs/badge.svg)](https://codecov.io/github/jranke/pfm) 

The R package **pfm** provides some utilities for fate modelling, including
simple routines for calculating predicted environmental concentrations (PEC)
and some routines for dealing with FOCUS pesticide fate modelling tools made
available under the GNU public license.

More specifically, **pfm** includes facilities for simple one-box modelling of
the [saw-tooth](https://pkgdown.jrwb.de/pfm/reference/sawtooth.html)-like
curves resulting from multiple repeated applications, for calculation
of [PEC soil](https://pkgdown.jrwb.de/pfm/reference/PEC_soil.html) based
on the 1997 SANCO guidance and the first tiers of the EFSA PEC soil guidance
from 2012 and 2015, as well as some functions for calculating [PEC surface 
water](https://pkgdown.jrwb.de/pfm/reference/sawtooth.html). 
The [PEC drift](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_drift.html)
calculations can be based on the Rautmann drift percentiles published by JKI,
the exponential formulas published by Rautmann to inter- or extrapolate to arbitrary
distances, or on the integrated Rautmann formulas (integrated over the width of the
surface water body) used in FOCUS drift calculations.

For PEC drainage calculations, the methods used by the [UK at tier 1](https://pkgdown.jrwb.de/pfm/reference/PEC_drainage_UK.html) and
by [Germany](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_exposit_drainage.html) are implemented.
For runoff, the
[German](https://pkgdown.jrwb.de/pfm/reference/PEC_sw_exposit_runoff.html) method used
at tier 1 is available.

The output of FOCUS TOXSWA calculations can be read in, plotted, and evaluated
using the [TOXSWA cwa](https://pkgdown.jrwb.de/pfm/reference/TOXSWA_cwa.html) class
giving maximum time weighted average concentrations and peak statistics  way as 
available when using the [EPAT](https://www.rifcon.com/en/downloads/software-2/)
tool.

## Installation

The easiest way to install the package is probably to use the 
[r-universe repo](https://jranke.r-universe.dev/pfm):

```r
install.packages("pfm",
  repos = c("https://jranke.r-universe.dev", "https://cran.r-project.org"))
```

The packages at R-universe are provided with a slight delay. Alternatively, you
can install the package directly from github, e.g. by using
[`pak`](https://pak.r-lib.org).

```r
# install.packages("pak")
pak::pak("jranke/pfm")
```

## Use

Please refer to the [reference](http://pkgdown.jrwb.de/pfm/reference/index.html).

## Examples

One nice example of the usage of this package is the visualisation
of a time weighted average for a sawtooth curve obtained from several overlays
of mkinfit predictions as shown [here](http://pkgdown.jrwb.de/pfm/reference/plot.one_box.html).

## Applications

Calculations of predicted environmental concentrations using this package have been used in some
publications by Agroscope.

<table>

  <tr><td>Korkaric M, Lehto M, Poiger T, de Baan L, Mathis M, Ammann L, Hanke I, Balmer M, Blom JF (2023)
  Risikoindikatoren für Pflanzenschutzmittel: weiterführende Analysen zur Berechnung.
  Agroscope Science, 154, 1-48, 
  <a href='https://doi.org/10.34776/as154g'>doi:10.34776/as154g</a>
  </td></tr>

  <tr><td>Korkaric M, Ammann L, Hanke I, Schneuwly J, Lehto M, Poiger T, de Baan L, Daniel O, Blom JF (2022)
  Neue Pflanzenschutzmittel-Risikoindikatoren für die Schweiz.
  Agrarforschung Schweiz 13, 1-10, 
  <a href='https://doi.org/10.34776/afs13-1'>doi:10.34776/afs13-1</a>
  </td></tr>

  <tr><td>Korkaric M, Hanke I, Grossar D, Neuweiler R, Christ B, Wirth J, Hochstrasser M, Dubuis PH, Kuster T, Breitenmoser S, Egger B, Perren S, Schürch S, Aldrich A, Jeker L, Poiger T, Daniel O (2020) Datengrundlage und Kriterien für eine Einschränkung der PSM-Auswahl im ÖLN: Schutz der Oberflächengewässer, der Bienen und des Grundwassers (Metaboliten), sowie agronomische Folgen der Einschränkungen.
  Agroscope Science, 106, 2020, 1-31.
  <a href='https://doi.org/10.34776/as106g'>doi:10.34776/as106g</a>
  </td></tr>

</table>

