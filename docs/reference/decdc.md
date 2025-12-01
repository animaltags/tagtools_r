# Reduce the sampling rate

This function is used to reduce the sampling rate of a time series by an
integer factor.

## Usage

``` r
decdc(x, df, go_slow = FALSE)
```

## Arguments

- x:

  A data structure, vector or matrix containing the signal(s) to be
  decimated. If x is a matrix, each column is decimated separately.

- df:

  The decimation factor. The output sampling rate is the input sampling
  rate divided by df. df must be an integer greater than 1.

- go_slow:

  if TRUE the function will NOT use Rcpp to speed up convolution.
  Default is FALSE. This input is for testing and benchmarking.

## Value

y The decimated signal vector or matrix. It has the same number of
columns as x but has 1/df of the rows.

## Note

Decimation is performed by first low-pass filtering x and then keeping 1
sample out of every df. A symmetric FIR filter with length 12\*df and
cutoff frequency 0.4\*fs/df is used. The group delay of the filter is
removed. For large decimation factors (e.g., df\>\>50), it is better to
perform several decimations with lower factors. For example to decimate
by 120, use: decdc(decdc(x,10),12).

## Examples

``` r
s <- matrix(sin(2 * pi / 100 * c(0:1000) - 1), ncol = 1)
plot(c(1:length(s)), s) 

y <- decdc(x = s, df = 4)
plot(c(1:length(y)), y) 

```
