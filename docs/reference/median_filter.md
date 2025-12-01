# Computes the nth-order median filter

This function computes the nth-order median filter each column of X. The
filter output is the median of each consecutive group of n samples. This
is useful for removing occasional outliers in data that is otherwise
fairly smooth. This makes it appropriate for pressure, temperature and
magnetometer data (amongst other sensors) but not so suitable for
acceleration which can be highly dynamic. The filter does not introduce
delay. The start and end values, i.e., within n samples of the start or
end of the input data, are computed with decreasing order median filters
unless noend = TRUE. If noend = TRUE, start and end values are taken
directly from X without short median filters.

## Usage

``` r
median_filter(X, n, noend = TRUE)
```

## Arguments

- X:

  A sensor list or a vector or matrix. If there are multiple columns in
  the data, each column is treated as a separate signal to be filtered.

- n:

  The filter length. If an even n is given, it is automatically
  incremented to make it odd. This ensures that the median is
  well-defined (the median of an even length vector is usually defined
  as the mean of the middle two points but may differ in different
  programmes). Note that a short n (e.g., 3 or 5) is usually sufficient
  and that processing will be very slow if n is large.

- noend:

  If TRUE (the default), then start and end values are taken directly
  from X without short median filters.

## Value

The output of the filter. It has the same size as S and has the same
sampling rate and units as X. If X is a sensor list, the return will
also be.

## Examples

``` r
v <- matrix(c(1, 3, 4, 4, 20, -10, 5, 6, 6, 7), ncol = 1)
w <- median_filter(v, n = 3)
```
