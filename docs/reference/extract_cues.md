# Extract multiple sub-samples of data

This function is used to extract multiple sub-samples of data from a
vector or matrix.

## Usage

``` r
extract_cues(x, sampling_rate, cues, len)
```

## Arguments

- x:

  is a vector or matrix of measurements. If x is a matrix, each column
  is treated as a separate measurement vector.

- sampling_rate:

  is the sampling rate in Hz of the data in x.

- cues:

  defines the start time in seconds of the intervals to be extracted
  from x.

- len:

  is the length of the interval to extract in seconds. This should be a
  scalar.

## Value

A list with 2 elements:

- **X:** A matrix containing sub-samples of x. If x is a vector, X has
  as many columns as there are cues, i.e., each cue generates a column
  of X. If x is a pxm matrix, X will be a qxmxn matrix where n is the
  size of cues and q is the length of the interval requested, i.e.,
  round(sampling_rate\*len) samples.

- **cues:** The list of cues actually used. cues that require data
  outside of x are rejected.

## Note

Output sampling rate is the same as the input sampling rate.

## Examples

``` r
BW <- beaked_whale # beaked_whale must be in your working directory
list <- extract_cues(x = BW$A$data, sampling_rate = BW$A$sampling_rate, cues = c(6, 40), len = 11)
```
