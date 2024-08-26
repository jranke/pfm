# pfm

[![pfm status badge](https://jranke.r-universe.dev/badges/pfm)](https://jranke.r-universe.dev/ui/#package:pfm)
[![Build Status](https://app.travis-ci.com/jranke/pfm.svg?token=Sq9VuYWyRz2FbBLxu6DK&branch=main)](https://app.travis-ci.com/jranke/pfm)
[![codecov](https://codecov.io/github/jranke/pfm/branch/main/graphs/badge.svg)](https://codecov.io/github/jranke/pfm) 

The R package **pfm** provides some utilities for fate modelling, including
dealing with FOCUS pesticide fate modelling tools, (currently only TOXSWA cwa
and out files), made available under the GNU public license.

## Installation

The easiest way to install the package is probably to use the 
[r-universe repo](https://jranke.r-universe.dev/pfm):

```r
install.packages("pfm", repos = c("https://jranke.r-universe.dev", "https://cran.r-project.org"))
```

Alternatively you can install the package 
using the `remotes` package.  Using `quick = TRUE` skips docs,
multiple-architecture builds, demos, and vignettes.

```r
remotes::install_github("jranke/pfm", quick = TRUE)
```

## Use

Please refer to the [reference](http://pkgdown.jrwb.de/pfm/reference/index.html).

## Examples

One recent nice example of the usage of this package is the visualisation
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

