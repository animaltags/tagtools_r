# Extract a sub-sample of data

This function is used to extract a sub-sample of data from a vector or
matrix.

## Usage

``` r
extract(x, sampling_rate, tst, ted)
```

## Arguments

- x:

  A vector or matrix of measurements. If x is a matrix, each column is
  treated as a separate measurement vector.

- sampling_rate:

  the sampling rate in Hz of the data in x.

- tst:

  Defines the start time in seconds of the interval to be extracted from
  x.

- ted:

  Defines the end time in seconds of the interval to be extracted from
  x.

## Value

**X:** A matrix containing a sub-sample of x. X has the same number of
columns as x. The length of the sub-sample will be
round(sampling_rate\*(tend-tstart)) samples.

## Note

Output sampling rate is the same as the input sampling rate.

If either tst or ted are beyond the length of x, non-existing samples
will be replaced with NaN in X.

## Examples

``` r
BW <- beaked_whale
BW_subset <- extract(x = BW$A$data, sampling_rate = BW$A$sampling_rate, tst = 3, ted = 100)
```
