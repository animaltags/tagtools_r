# Helper function for plott

This internal function helps`plott` produce the individual panels that
make up the static ggplot output. It will not usually be needed by
users.

## Usage

``` r
plott_static_panel(
  sensor,
  sensor_data,
  line_colors,
  panel_labels,
  axis_names = c("X", "Y", "Z"),
  times,
  x_lab,
  r
)
```

## Arguments

- sensor:

  Which sensor to plot

- sensor_data:

  List whose elements are either lists (containing data and metadata) or
  vectors/matrices of time series data. See details.

- line_colors:

  Vector of colors to use for lines

- panel_labels:

  Labels for each panel (sensor)

- axis_names:

  Names for different axes for sensors with more than one (for example:
  X, Y, Z)

- times:

  List of vectors of times for the x axis of each panel (one list
  element per panel)

- x_lab:

  Title for x axis (string)

- r:

  whether or not to reverse the y axis scale for each sensor

## Value

A ggplot for "one panel," that is, with the data from one sensor
