# pfm

The R package **pfm** provides some utilities for fate modelling, including
dealing with FOCUS pesticide fate modelling tools, (currently only TOXSWA cwa
and out files), made available under the GNU public license.
This means:

    This program is free software: you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any later
    version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
    details.

    You should have received a copy of the GNU General Public License along with
    this program. If not, see <http://www.gnu.org/licenses/>

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

Another, even more recent example shows how FOCUS PELMO can be run in parallel under Linux
as shown [here](http://pkgdown.jrwb.de/pfm/reference/PELMO_runs.html).
