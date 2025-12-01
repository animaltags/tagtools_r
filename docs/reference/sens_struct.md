# Generate a sensor structure from a sensor data vector or matrix.

Generate a sensor structure from a sensor data vector or matrix.

## Usage

``` r
sens_struct(
  data,
  sampling_rate = NULL,
  times = NULL,
  depid,
  type,
  unit = NULL,
  frame = NULL,
  name = type,
  description = NULL,
  start_offset = 0,
  start_offset_units = "second",
  quiet = FALSE
)
```

## Arguments

- data:

  sensor data vector or matrix

- sampling_rate:

  (optional) sensor data sampling rate in Hz

- times:

  (optional) is the time in seconds of each measurement in data for
  irregularly sampled data. The time reference (i.e., the 0 time) should
  be with respect to the start time of the deployment.

- depid:

  string that provides a unique identifier for this tag deployment

- type:

  is a string containing the first few letters of the sensor type, e.g.,
  acc for acceleration. These will be matched to the list of sensor
  types in the sensor_names.csv file. If more than one sensor matches
  type, a warning will be given. type can be in upper or lower case.

- unit:

  (optional) units in which data are sampled. Default determined by
  matching `type` with defaults in sensor_names.csv

- frame:

  (optional) frame of reference for data axes, for example 'animal' or
  'tag'. Default determined by matching `type` with defaults in
  sensor_names.csv.

- name:

  (optional) "full name" to assign to the variable. Default determined
  by matching `type` to defaults in sensor_names.csv/

- description:

  (optional) more detail about this sensor (if provided will override a
  default explanation)

- start_offset:

  (optional) offset in start time for this sensor relative to start of
  tag recording. Defaults to 0.

- start_offset_units:

  (optional) units of start_offset. default is 'second'.

- quiet:

  prints to screen if quiet is set to false

## Value

A sensor list with field `data` containing the data and with metadata
fields pre-populated from the sensor_names.csv file. Change these
manually as needed (or specify the relevant inputs to `sens_struct`) to
the correct values.

## Examples

``` r
HB <- harbor_seal
A <- sens_struct(data=HB$A$data,sampling_rate=3,depid='md13_134a', type='acc', quiet=TRUE)
```
