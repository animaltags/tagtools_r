# Implement a calibration on tag sensor data

Given an appropriate set of calibration constants and information, this
function will apply the calibration procedure to a tag sensor data set.
Cal fields currently supported are: poly, cross, map, tcomp, tref

## Usage

``` r
apply_cal(X, cal, Tempr = NULL)
```

## Arguments

- X:

  A tag sensor data list, or a matrix or vector containing tag sensor
  data

- cal:

  A calibration list for the data in X from, for example, spherical_cal.

- Tempr:

  a tag sensor data list or a vector of temperature measurements for use
  in temperature compensation. If Tempr is not a sensor data list, it
  must be the same size and sampling rate as the data in `X`. Tempr is
  only required if there is a tcomp item in the `cal` list.

## Value

A tag sensor data structure (or a matrix or vector, if X was a matrix or
vector) with the calibration implemented. Data size and sampling rate
are the same as for the input data `X`, but units may have changed.

## Examples

``` r
A_cal <- apply_cal(harbor_seal$A,spherical_cal(harbor_seal$A$data))
```
