# Calculate total acceleration

This function calculates the total acceleration due to gravitation and
centripetal force at the earth's surface according to the WGS84
international gravity formula.

## Usage

``` r
acc_wgs84(latitude)
```

## Arguments

- latitude:

  The latitude in degrees.

## Value

g given in units of \$m/s^2\$

## Note

Source: http://solid_earth.ou.edu/notes/potential/igf.htm

## Examples

``` r
acc_wgs84(50)
#> [1] 9.810704
```
