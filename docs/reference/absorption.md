# Calculates the absorption coefficient for sound in seawater

Calculates the absorption coefficient for sound in seawater

## Usage

``` r
absorption(freq, temperature, d)
```

## Arguments

- freq:

  frequency in Hz

- temperature:

  temperature in degrees C

- d:

  depth in meters

## Value

The sound absorption in dB per metre.

## Note

Input arguments can be scalars, or a mixture of vectors and scalars as
long as each argument is either a vector of length nx1 (with n being the
same for all vector arguments) or a scalar.

After Kinsler and Frey pp. 159-160

## Examples

``` r
absorption(140e3, 13, 10)
#> [1] 0.04354982
```
