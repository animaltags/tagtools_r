# Compute mean of sample blocks

This function is used to compute the means of successive blocks of
samples.

## Usage

``` r
block_mean(X, n, nov)
```

## Arguments

- X:

  A vector or a matrix containing samples of a signal in each column.

- n:

  The number of samples from X to use in each analysis block.

- nov:

  (optional) The number of samples that the next block overlaps the
  previous block. The default value is 0.

## Value

A list with 2 elements:

- **Y:** A vector or matrix containing the mean value of each block. If
  X is a mxn matrix, Y is pxn where p is the number of complete n-length
  blocks with nov that can be made out of m samples, i.e.,
  n+(p-1)\*(n-nov) \< m

- **samples:** The time at which each output in Y is reported, in units
  of samples of X. So if samples\[1\] = 12, then the value Y\[1\]
  corresponds to the “time” 12 samples in X.

## Examples

``` r
samplematrix <- matrix(c(1, 3, 5, 7, 9, 11, 13, 15, 17), byrow = TRUE, ncol = 3)
list <- block_mean(samplematrix, n = 3, nov = 1)
```
