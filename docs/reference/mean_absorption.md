# Calculate the mean absorption in salt water

This function is used to calculate the mean absorption in salt water
over a frequency range.

## Usage

``` r
mean_absorption(freq, r, depth, Ttab = 13)
```

## Arguments

- freq:

  Specifies the frequency range, freq = c(fmin, fmax) in Hz. For a
  single frequency, use a scalar value for freq.

- r:

  The path (slant) length in metres.

- depth:

  The depths covered by the path. This can be a single value for a
  horizontal path or a two component vector i.e., depth = c(dmax, dmin)
  for a path that extends between two depths.

- Ttab:

  (optional) The temperature (a scalar) in degrees C or specifies a
  temperature profile Ttab = c(depth, tempr) where depth and tempr are
  equal-sized column vectors. Default value is an isothermal profile of
  13 degrees.

## Value

The mean sound absorption over the path in dB.

## Note

After Kinsler and Frey pp. 159-160.

## Examples

``` r
mean_absorption(c(25e3, 60e3), 1000, c(0, 700))
#>          [,1]
#> [1,] 7.728188
```
