# pfm

[![Build Status](https://travis-ci.com/jranke/pfm.svg?branch=main)](https://app.travis-ci.com/github/jranke/pfm)
[![pfm status badge](https://jranke.r-universe.dev/badges/pfm)](https://jranke.r-universe.dev/ui/#package:pfm)
[![codecov](https://codecov.io/github/jranke/pfm/branch/main/graphs/badge.svg)](https://codecov.io/github/jranke/pfm) 

The R package **pfm** provides some utilities for fate modelling, including
dealing with FOCUS pesticide fate modelling tools, (currently only TOXSWA cwa
and out files), made available under the GNU public license.

## Installation

The easiest way to install the package is probably to use 
[drat](https://cran.r-project.org/package=drat):

```r
install.packages("drat")
drat::addRepo("jranke")
install.packages("pfm")
```

Alternatively you can install the package 
using the `devtools` package.  Using `quick = TRUE` skips docs,
multiple-architecture builds, demos, and vignettes.


```r
library(devtools)
install_github("jranke/pfm", quick = TRUE)
```

## Use

Please refer to the [reference](http://pkgdown.jrwb.de/pfm/reference/index.html).

## Examples

One recent nice example of the usage of this package is the visualisation
of a time weighted average for a sawtooth curve obtained from several overlays
of mkinfit predictions as shown [here](http://pkgdown.jrwb.de/pfm/reference/plot.one_box.html).
