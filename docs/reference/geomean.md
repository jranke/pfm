# Calculate the geometric mean

Based on some posts in a thread on Stackoverflow
<http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in>
This function returns NA if NA values are present and na.rm = FALSE
(default). If negative values are present, it gives an error message. If
at least one element of the vector is 0, it returns 0.

## Usage

``` r
geomean(x, na.rm = FALSE)
```

## Arguments

- x:

  Vector of numbers

- na.rm:

  Should NA values be omitted?

## Value

The geometric mean

## Author

Johannes Ranke

## Examples

``` r
geomean(c(1, 3, 9))
#> [1] 3
geomean(c(1, 3, NA, 9))
#> [1] NA
geomean(c(1, -3, 9)) # returns an error
#> Error in geomean(c(1, -3, 9)): Only defined for positive numbers
```
