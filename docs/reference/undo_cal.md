# Undo calibrations steps

This function is used to undo any calibration steps that have been
applied to sensor data. This will reverse any re-mapping, scaling and
offset adjustments that have been applied to the data, reverting the
sensor data to the state it was when read in from the source (excluding
any filtering or decimation steps).

## Usage

``` r
undo_cal(X, temperature)
```

## Arguments

- X:

  A sensor list or set of sensor lists in the tag frame, i.e., with
  calibrations applied.

- temperature:

  A vector of temperature measurements with the same number of samples
  and sampling rate as the data in the input sensor data structure X.
  The temperature parameter indicates the temperature experienced by the
  sensor during data collection (not necessarily the ambient temperature
  experienced by the animal), and may affect calibration because many
  sensors’ output values change depending on the temperature.

## Value

A sensor list or set of sensor lists reverted to the sensor frame, i.e.,
without calibrations.

## Examples

``` r
BW <- beaked_whale
no_cal <- undo_cal(BW)
```
