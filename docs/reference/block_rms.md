# Compute RMS of sample blocks

This function is used to compute the RMS (root-mean-square) of
successive blocks of samples.

## Usage

``` r
block_rms(X, n, nov = NULL)
```

## Arguments

- X:

  A vector or a matrix containing samples of a signal in each column.

- n:

  The number of samples from X to use in each analysis block.

- nov:

  The number of samples that the next block overlaps the previous block.

## Value

A list with 2 elements:

- **Y:** A vector or matrix containing the RMS value of each block. If X
  is a mxn matrix, Y is pxn where p is the number of complete n-length
  blocks with nov that can be made out of m samples, i.e.,
  n+(p-1)\*(n-nov) \< m

- **samples:** The time at which each output in Y is reported, in units
  of samples of X. So if samples\[1\] = 12, then the value Y\[1\]
  corresponds to the “time” 12 samples in X. The times at which Y values
  are reported are the centers of the averaging windows.

## Note

Output sampling rate is the same as the input sampling rate so s and v
have the same size as p.

Frame: This function assumes a \[north,east,up\] navigation frame and a
\[forward,right,up\] local frame. In these frames, a positive pitch
angle is an anti-clockwise rotation around the y-axis. A descending
animal will have a negative pitch angle.

## Examples

``` r
X <- matrix(c(1:20), byrow = TRUE, nrow = 4)
block_rms(X, n = 2, nov = NULL)
#> $Y
#>           [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,]  4.301163  5.147815  6.041523  6.964194  7.905694
#> [2,] 13.729530 14.713939 15.700318 16.688319 17.677670
#> 
#> $samples
#> [1] 1 3
#> 
```
