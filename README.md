# pfm

The R package **pfm** provides some utilities for dealing with FOCUS pesticide fate modelling tools,
(currently only TOXSWA cwa and out files), made available under the GNU public license.
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

You can install the package from [github](http://github.com/jranke/pfm), e.g.
using the `devtools` package.  Using `quick = TRUE` skips docs,
multiple-architecture builds, demos, and vignettes, to make installation as
fast and painless as possible.


```r
library(devtools)
install_github("jranke/pfm", subdir = "pkg", quick = TRUE)
```

## Use

### Analyse TOXSWA output

Read in and analyse a cwa file:



```r
library(pfm, quietly = TRUE)
```

```
## 
## Initialize Python Version 2.7.9 (default, Jun 29 2016, 13:11:10) 
## [GCC 4.9.2]
```

```r
example_cwa <- read.TOXSWA_cwa("00003s_pa.cwa")
plot(example_cwa)
```

<img src="README_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Get events above thresholds of 20 and 100 µg/L,
and do a moving window analysis for windows of 7 days
and 21 days, print the results:


```r
example_cwa$get_events(c(20, 100))
example_cwa$moving_windows(c(7, 21))
print(example_cwa)
```

```
## <TOXSWA_cwa> data from file 00003s_pa.cwa segment 20 
##                datetime     t t_firstjan t_rel_to_max cwa_mug_per_L
## 20  1978-10-01 00:00:00 0.000   273.0000      -55.333             0
## 40  1978-10-01 01:00:00 0.042   273.0417      -55.291             0
## 60  1978-10-01 02:00:00 0.083   273.0833      -55.250             0
## 80  1978-10-01 03:00:00 0.125   273.1250      -55.208             0
## 100 1978-10-01 04:00:00 0.167   273.1667      -55.166             0
## 120 1978-10-01 05:00:00 0.208   273.2083      -55.125             0
##     cwa_tot_mug_per_L
## 20                  0
## 40                  0
## 60                  0
## 80                  0
## 100                 0
## 120                 0
## Moving window analysis
##    window  max_TWAC max_AUC_h max_AUC_d
## 1  7 days 2.3926551  401.9660  16.74859
## 2 21 days 0.8369248  421.8101  17.57542
## Event statistics for threshold 20 
##   t_start  cwa_max duration pre_interval    AUC_h   AUC_d
## 1  55.083 40.58401    0.417       55.083 365.7912 15.2413
## Event statistics for threshold 100 
## No events found
```

This can also be done with out files, the function reads
out files from current TOXSWA versions as well as cwa files
from old TOXSWA versions.


### Calculate PEC soil

Simple PEC soil calculation for an application rate of 100 g/ha and
25% interception, assuming complete mixing into 5 cm and a soil bulk
density of 1.5 kg/L, output in mg/kg:



```r
PEC_soil(100, interception = 0.25)
```

```
##      scenario
## t_avg default
##     0     0.1
```

### Rautmann drift data

Some of the drift percentage data published by the JKI are included. To
see the data for one application:



```r
drift_data_JKI[1]
```

```
## [[1]]
##         crop
## distance Ackerbau Obstbau frueh Obstbau spaet
##       1      2.77            NA            NA
##       3        NA         29.20         15.73
##       5      0.57         19.89          8.41
##       10     0.29         11.81          3.60
##       15     0.20          5.55          1.81
##       20     0.15          2.77          1.09
##       30     0.10          1.04          0.54
##       40     0.07          0.52          0.32
##       50     0.06          0.30          0.22
```

### PEC surface water due to drift

Initial PEC values for an application of 100 g/ha in the vicinity of a 30 cm
deep water body are obtained using



```r
PEC_sw_drift(100, applications = 1)
```

```
##        1 m        5 m       10 m       20 m 
## 0.92333333 0.19000000 0.09666667 0.05000000
```
