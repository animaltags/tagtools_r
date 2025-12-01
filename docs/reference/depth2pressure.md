# Convert depth to pressure

This function is used to convert the depth (in meters) to the pressure
in Pascals.

## Usage

``` r
depth2pressure(d, latitude)
```

## Arguments

- d:

  The depth in meters

- latitude:

  The latitude in degrees

## Value

The pressure in Pa

## Note

Based on the Leroy and Parthiot (1998) formula. See:
http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html#UNESCO

## Examples

``` r
depth2pressure(1000, 27)
#> [1] 10075403
```
