# Sound speed estimation

This function is used to estimate the sound speed using Coppens equation

## Usage

``` r
sound_speed(temperature, D = NULL, S = NULL)
```

## Arguments

- temperature:

  The temperature in degrees C

- D:

  (optional) The depth in meters (defaults to 1 m)

- S:

  The salinity in part-per-thousand (defaults to 35 ppt)

## Value

The sound speed in m/s

## Note

Range of validity: temperature 0 to 35 °C, salinity 0 to 45 parts per
thousand, depth 0 to 4000 m

Source:
http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html#UNESCO

## Examples

``` r
sound_speed(8, 1000, 34)
#> [1] 1497.708
```
