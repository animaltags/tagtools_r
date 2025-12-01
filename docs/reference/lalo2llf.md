# Convert latitude-longitude track points into a local level frame

Convert latitude-longitude track points into a local level frame

## Usage

``` r
lalo2llf(trk, pt = NULL)
```

## Arguments

- trk:

  A data frame, two-column matrix, two-element vector of track points
  c(latitude, longitude) or sensor data structure.

- pt:

  c(latitude, longitude) of the centre point of the local level frame.
  If pt is not given, the first point in the track will be used.

## Value

A data frame with columns `northing` and `easting` of track points in
the local level frame. Northing and easting are in metres. The axes of
the frame are true (geographic) north and true east.

## Note

This function assumes the track is on the surface of the geoid, and also
uses a simple spherical model for the geoid. For more accurate
conversion to a Cartesian frame, use spatial and mapping packages in
Matlab/Octave.

## Examples

``` r
coordinates <- matrix(c(
-122.4194, 37.7749,
-73.9352,  40.7306), nrow = 2, ncol = 2, byrow = TRUE)
lalo2llf(coordinates, c(15,19))
#>    northing easting
#> 1 -15270044 2015179
#> 2  -9882479 2332425
```
