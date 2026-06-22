# Calculate a time weighted average concentration

The moving average is built only using the values in the past, so the
earliest possible time for the maximum in the time series returned is
after one window has passed.

## Usage

``` r
twa(x, window = 21)

# S3 method for class 'one_box'
twa(x, window = 21)
```

## Arguments

- x:

  An object of type
  [one_box](https://pkgdown.jrwb.de/pfm/reference/one_box.md)

- window:

  The size of the moving window

## See also

[`max_twa`](https://pkgdown.jrwb.de/pfm/reference/max_twa.md)

## Examples

``` r
pred <- sawtooth(one_box(10),
  applications = data.frame(time = c(0, 7), amount = c(1, 1)))
max_twa(pred)
#> $max
#>    parent 
#> 0.9537545 
#> 
#> $window_start
#> parent 
#>      0 
#> 
#> $window_end
#> parent 
#>     21 
#> 
```
