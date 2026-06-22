# Deposition from spray drift expressed as percent of the applied dose as published by the JKI

Deposition from spray drift expressed as percent of the applied dose as
published by the German Julius-Kühn Institute (JKI).

## Usage

``` r
drift_data_JKI
```

## Format

A list currently containing matrices with spray drift percentage data
for field crops (Ackerbau), and Pome/stone fruit, early and late
(Obstbau frueh, spaet).

## Source

JKI (2010) Spreadsheet 'Tabelle der Abdrifteckwerte.xls', retrieved from
http://www.jki.bund.de/no_cache/de/startseite/institute/anwendungstechnik/abdrift-eckwerte.html
on 2015-06-11, not present any more 2024-01-31

Rautmann, D., Streloke, M and Winkler, R (2001) New basic drift values
in the authorization procedure for plant protection products Mitt. Biol.
Bundesanst. Land- Forstwirtsch. 383, 133-141

## Details

The data were extracted from the spreadsheet cited below using the R
code given in the file `data_generation/drift_data_JKI.R` installed with
this package. The file itself is not included in the package, as its
licence is not clear.

Additional spray drift values were taken from the publication by
Rautmann et al. (2001). Specifically, these are the values for early
vines, and the values for a 3 m buffer which are incomplete in the
spreadsheet.

Note that for vegetables, ornamentals and small fruit, the values for
field crops are used for crops \< 50 cm, and the vales for late vines
are used for crops \> 50 cm. In the JKI spreadsheet, it is indicated
that these values are used for spray applications with handheld/knapsack
equipment (tragbare Spritz- und Sprühgerate).

Values for non-professional use listed in the JKI spreadsheet were not
included.

## Examples

``` r
drift_data_JKI
#> [[1]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      2.77            NA            NA            NA            NA
#>       3      0.95         29.20         15.73          2.70          8.02
#>       5      0.57         19.89          8.41          1.18          3.62
#>       10     0.29         11.81          3.60          0.39          1.23
#>       15     0.20          5.55          1.81          0.20          0.65
#>       20     0.15          2.77          1.09          0.13          0.42
#>       30     0.10          1.04          0.54          0.07          0.22
#>       40     0.07          0.52          0.32          0.04          0.14
#>       50     0.06          0.30          0.22          0.03          0.10
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       4.440           NA
#>       3      19.33                          NA  0.018721696
#>       5      11.57                       0.180  0.014363896
#>       10      5.77                       0.050  0.010026007
#>       15      3.84                       0.020  0.008124366
#>       20      1.79                       0.012  0.006998158
#>       30      0.56                       0.005  0.005670811
#>       40      0.25                       0.003           NA
#>       50      0.13                       0.002  0.004350831
#> 
#> [[2]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      2.38            NA            NA            NA            NA
#>       3      0.79         25.53         12.13          2.53          7.23
#>       5      0.47         16.87          6.81          1.09          3.22
#>       10     0.24          9.61          3.11          0.35          1.07
#>       15     0.16          5.61          1.58          0.18          0.56
#>       20     0.12          2.59          0.90          0.11          0.36
#>       30     0.08          0.87          0.40          0.06          0.19
#>       40     0.06          0.40          0.23          0.03          0.12
#>       50     0.05          0.22          0.15          0.02          0.08
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       3.780           NA
#>       3      17.73                          NA           NA
#>       5       9.60                       0.160           NA
#>       10      4.18                       0.040           NA
#>       15      2.57                       0.020           NA
#>       20      1.21                       0.011           NA
#>       30      0.38                       0.005           NA
#>       40      0.17                       0.003           NA
#>       50      0.09                       0.002           NA
#> 
#> [[3]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      2.01            NA            NA            NA            NA
#>       3      0.68         23.96         11.01          2.49          6.90
#>       5      0.41         15.79          6.04          1.04          3.07
#>       10     0.20          8.96          2.67          0.32          1.02
#>       15     0.14          4.24          1.39          0.16          0.54
#>       20     0.10          2.01          0.80          0.10          0.34
#>       30     0.07          0.70          0.36          0.05          0.18
#>       40     0.05          0.33          0.21          0.03          0.11
#>       50     0.04          0.19          0.13          0.02          0.08
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       3.420           NA
#>       3      15.93                          NA           NA
#>       5       8.57                       0.150           NA
#>       10      3.70                       0.040           NA
#>       15      2.26                       0.020           NA
#>       20      1.05                       0.010           NA
#>       30      0.34                       0.004           NA
#>       40      0.15                       0.003           NA
#>       50      0.08                       0.002           NA
#> 
#> [[4]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      1.85            NA            NA            NA            NA
#>       3      0.62         23.61         10.12          2.44          6.71
#>       5      0.38         15.42          5.60          1.02          2.99
#>       10     0.19          8.66          2.50          0.31          0.99
#>       15     0.13          4.01          1.28          0.16          0.52
#>       20     0.10          1.89          0.75          0.10          0.33
#>       30     0.06          0.66          0.35          0.05          0.17
#>       40     0.05          0.31          0.20          0.03          0.11
#>       50     0.04          0.17          0.13          0.02          0.08
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       2.290           NA
#>       3      15.38                          NA           NA
#>       5       8.26                       0.120           NA
#>       10      3.55                       0.030           NA
#>       15      2.17                       0.020           NA
#>       20      0.93                       0.009           NA
#>       30      0.31                       0.004           NA
#>       40      0.14                       0.002           NA
#>       50      0.08                       0.002           NA
#> 
#> [[5]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      1.75            NA            NA            NA            NA
#>       3      0.59         23.12          9.74          2.37          6.59
#>       5      0.36         15.06          5.41          1.00          2.93
#>       10     0.18          8.42          2.43          0.31          0.98
#>       15     0.12          3.83          1.24          0.15          0.51
#>       20     0.09          1.81          0.72          0.09          0.33
#>       30     0.06          0.63          0.34          0.05          0.17
#>       40     0.05          0.30          0.20          0.03          0.11
#>       50     0.04          0.17          0.13          0.02          0.08
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       2.120           NA
#>       3      15.12                          NA           NA
#>       5       7.99                       0.110           NA
#>       10      3.36                       0.030           NA
#>       15      2.03                       0.010           NA
#>       20      0.88                       0.008           NA
#>       30      0.29                       0.004           NA
#>       40      0.14                       0.002           NA
#>       50      0.07                       0.002           NA
#> 
#> [[6]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      1.64            NA            NA            NA            NA
#>       3      0.56         22.76          9.21          2.29          6.41
#>       5      0.34         14.64          5.18          0.97          2.85
#>       10     0.17          8.04          2.38          0.30          0.95
#>       15     0.11          3.71          1.20          0.15          0.50
#>       20     0.09          1.75          0.68          0.09          0.32
#>       30     0.06          0.61          0.31          0.05          0.17
#>       40     0.04          0.29          0.17          0.03          0.11
#>       50     0.03          0.16          0.11          0.02          0.07
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       1.980           NA
#>       3      14.90                          NA           NA
#>       5       7.79                       0.100           NA
#>       10      3.23                       0.030           NA
#>       15      1.93                       0.010           NA
#>       20      0.83                       0.008           NA
#>       30      0.28                       0.004           NA
#>       40      0.13                       0.002           NA
#>       50      0.07                       0.001           NA
#> 
#> [[7]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      1.61            NA            NA            NA            NA
#>       3      0.55         22.69          9.10          2.24          6.33
#>       5      0.33         14.45          5.11          0.94          2.81
#>       10     0.17          7.83          2.33          0.29          0.94
#>       15     0.11          3.62          1.20          0.15          0.49
#>       20     0.08          1.71          0.67          0.09          0.31
#>       30     0.06          0.60          0.30          0.05          0.16
#>       40     0.04          0.28          0.17          0.03          0.10
#>       50     0.03          0.16          0.11          0.02          0.07
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       1.930           NA
#>       3      14.63                          NA           NA
#>       5       7.60                       0.100           NA
#>       10      3.13                       0.030           NA
#>       15      1.86                       0.010           NA
#>       20      0.81                       0.008           NA
#>       30      0.26                       0.004           NA
#>       40      0.12                       0.002           NA
#>       50      0.06                       0.001           NA
#> 
#> [[8]]
#>         crop
#> distance Ackerbau Obstbau frueh Obstbau spaet Weinbau frueh Weinbau spaet
#>       1      1.52            NA            NA            NA            NA
#>       3      0.52         22.24          8.66          2.16          6.26
#>       5      0.31         14.09          4.92          0.91          2.78
#>       10     0.16          7.58          2.29          0.28          0.93
#>       15     0.11          3.48          1.14          0.14          0.49
#>       20     0.08          1.65          0.65          0.09          0.31
#>       30     0.05          0.57          0.29          0.04          0.16
#>       40     0.04          0.27          0.16          0.03          0.10
#>       50     0.03          0.15          0.11          0.02          0.07
#>         crop
#> distance Hopfenbau Flaechenkulturen > 900 l/ha Gleisanlagen
#>       1         NA                       1.640           NA
#>       3      13.53                          NA           NA
#>       5       7.15                       0.090           NA
#>       10      3.01                       0.020           NA
#>       15      1.82                       0.010           NA
#>       20      0.78                       0.007           NA
#>       30      0.25                       0.003           NA
#>       40      0.12                       0.002           NA
#>       50      0.06                       0.001           NA
#> 
```
