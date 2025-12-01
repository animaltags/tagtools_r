# Plot tag data time series

Plot time series in a single or multi-paneled figure, using base R
graphics. This is useful, for example, for comparing measurements across
different sensors in an animaltag data object. The time axis is
automatically displayed in seconds, minutes, hours, or days according to
the span of the data.

## Usage

``` r
plott_base(
  X,
  fsx = NULL,
  r = FALSE,
  offset = 0,
  date_time_axis = FALSE,
  recording_start = NULL,
  panel_heights = rep.int(1, length(X)),
  panel_labels = names(X),
  line_colors,
  interactive = FALSE,
  par_opts,
  ...
)
```

## Arguments

- X:

  List whose elements are either lists (containing data and metadata) or
  vectors/matrices of time series data. See details.

- fsx:

  (Optional) A numeric vector whose length matches the number of sensor
  data streams (list elements) in X. (If shorter, `fsx` will be recycled
  to the appropriate length). `fsx` gives the sampling rate in Hz for
  each data object. Sampling rates are not needed when the data
  object(s) `X` are list(s) that contain sampling rate information – and
  beware, because `fsx` (if given) will override sensor metadata.

- r:

  (Optional) Logical. Should the direction of the y-axis be flipped?
  Default is FALSE. If `r` is of length one (or shorter than the number
  of sensor data streams in X) it will be recycled to match the number
  of sensor data streams. Reversed y-axes are useful, for example, for
  plotting dive profiles which match the physical situation (with
  greater depths lower in the display). If the name of a sensor list is
  "P" or contains the word "depth", it will automatically be reversed.

- offset:

  (Optional) A vector of offsets, in seconds, between the start of each
  sensor data stream and the start of the first one. For example, if
  acceleration data collection started and then depth data collection
  commenced 436 seconds later, then the `offset` for the depth data
  would be 436.

- date_time_axis:

  (Optional) Logical. Should the x-axis units be date-times rather than
  time-since-start-of-recording? Ignored if `recording_start` is not
  provided and `X` does not contain metadata on recording start time.
  Default is FALSE.

- recording_start:

  (Optional) The start time of the tag recording as a
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) object. If
  provided, the time axis will show calendar date/times; if not, it will
  show days/hours/minutes/seconds (as appropriate) since time 0 = the
  start of recording. If a character string is provided it will be
  coerced to POSIXct with
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).

- panel_heights:

  (Optional) A vector of relative or absolute heights for the different
  panels (one entry for each sensor data stream in `X`). Default is
  equal-height panels. If `panel_heights` is a numeric vector, it is
  interpreted as relative panel heights. To specify absolute panel
  heights in centimeters using `lcm` (see help for
  [`layout`](https://rdrr.io/r/graphics/layout.html)).

- panel_labels:

  (Optional) A list of y-axis labels for the panels. Defaults to
  names(X).

- line_colors:

  (Optional) A list of colors for lines for multivariate data streams
  (for example, if a panel plots tri-axial acceleration, it will have
  three lines – their line colors will be the first three in this list).
  May be specified in any specification R understands for colors.
  Defaults to c("#000000", "#009E73", "#9ad0f3", "#0072B2", "#e79f00",
  "#D55E00")

- interactive:

  (Optional) Should an interactive figure (allowing zoom/pan/etc.) be
  produced? Default is FALSE. Interactive plotting requires the zoom
  package for its [`zm`](https://rdrr.io/pkg/zoom/man/zm.html) function.

- par_opts:

  (Optional) A list of options to be passed to
  [`par`](https://rdrr.io/r/graphics/par.html) before plotting. Default
  is mar=c(1,5,0,0), oma=c(2,0,2,1), las=1, lwd=1, cex=0.8.

- ...:

  Additional arguments to be passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

A plot of time-series data

## Details

If the input data X is an `animaltag` object, then all sensor variables
in the object will be plotted. To plot only selected sensors from the
`animaltag` object `my_tag`, for example, the input X=list(my_tag\$A,
my_tag\$M) would plot just the accelerometer and magnetometer data. If
possible, the plot will have

## Note

This is a flexible plotting tool which can be used to display and
explore sensor data with different sampling rates on a uniform time
grid.

## Examples

``` r
plott_base(list(depth = harbor_seal$P, Accel = harbor_seal$A))

```
