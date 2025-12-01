# Remove NAs from sensor data and return indices of (rows of) filled values

This is an internal function used by
[`htrack`](https://animaltags.github.io/tagtools_r/reference/htrack.md)

## Usage

``` r
interp_nan(data)
```

## Arguments

- data:

  a data vector or matrix

## Value

A list with entries `data` (the input data with NAs filled in) and `k` a
logical vector indicating the position (if data was a vector) or rows
(if data was a matrix) where NAs were filled in. Internal NAs are filled
via linear interoplation, while leading and trailing ones are filled
using the first following or last preceding good value.

## Examples

``` r
A <- matrix(c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, 13, 14, 15, 16, NA, NA), ncol = 2)
result <- interp_nan(A)
```
