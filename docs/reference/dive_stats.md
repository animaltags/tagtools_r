# Compute summary statistics for dives or flights

Given a depth/altitude profile and a series of dive/flight start and end
times, compute summary dive statistics.

## Usage

``` r
dive_stats(
  P,
  X = NULL,
  dive_cues,
  sampling_rate = NULL,
  prop = 0.85,
  angular = FALSE,
  X_name = NULL,
  na.rm = TRUE
)
```

## Arguments

- P:

  Depth data. A vector (or one-column matrix), or a tag sensor data
  list.

- X:

  (optional) Another data stream (as a vector (or a one-column matrix)
  or a tag sensor data list) for which to compute mean and variability.
  If `angular` is TRUE, interpreted as angular data (for example pitch,
  roll, or heading) and means and variances are computed accordingly.
  The unit of measure must be radians (NOT degrees). Currently, `X` must
  be regularly sampled.

- dive_cues:

  A two-column data frame or matrix with dive/flight start times in the
  first column and dive/flight end times in the second. May be obtained
  from
  [`find_dives`](https://animaltags.github.io/tagtools_r/reference/find_dives.md).
  Units should be seconds since start of tag recording.

- sampling_rate:

  (optional and ignored if `P` or `X` are tag sensor data lists)
  Sampling rate of `P` (and `X`, if `X` is given). If omitted, then
  input data must be sensor data lists. If one value is given and both
  `P` and `X` are input, they are assumed to have the same sampling
  rate. If `P` and `X` have different sampling rates, then this input
  can have two elements (first for `P`, second for `X`).

- prop:

  The proportion of the maximal excursion to use for defining the
  "destination" phase of a dive or flight. For example, if `prop` is
  0.85 (the default), then the destination phase lasts from the first to
  the last time depth/altitude exceeds 0.85 times the within-dive
  maximum.

- angular:

  Is X angular data? Defaults to FALSE.

- X_name:

  A short name to use for X variable in the output data frame. For
  example, if X is pitch data, use X_name='pitch' to get outputs column
  names like mean_pitch, etc. Defaults to 'angle' for angular data and
  'aux' for non-angular data.

- na.rm:

  Logical, default is TRUE. If TRUE, then returned mean values ignore
  missing values, computing an average over all non-missing
  observations.

## Value

A data frame with one row for each dive/flight and columns as detailed
below. All times are in seconds, and rates in units of x/sec where x is
the units of `P`.

- `max` The maximum depth or altitude

- `st` start time of dive (seconds) - from input dive_cues

- `et` end time of dive (seconds) - from input dive_cues

- `dur` The duration of the excursion

- `dest_st` The start time of the destination phase in seconds since
  start of tag recording (which is also the end time of to phase)

- `dest_et` The end time of the destination phase in seconds since start
  of tag recording (which is also the start of the from phase).

- `dest_dur` The duration in seconds of destination phase

- `to_dur` The duration in seconds of to phase

- `from_dur` The duration in seconds of from phase

- `mean_angle` If angular=TRUE and X is input, the mean angle for the
  entire excursion. Values for each phase are also provided in columns
  `mean_to_angle`, `mean_dest_angle`, and `mean_from_angle`.

- `angle_var` If angular=TRUE and X is input, the angular variance for
  the entire excursion. Values for each phase are also provided
  individually in columns `to_angle_var`, `dest_angle_var`, and
  `from_angle_var`.

- `mean_aux` If angular=FALSE and X is input, the mean value of X for
  the entire excursion. Values for each phase are also provided in
  columns `mean_to_aux`, `mean_dest_aux`, and `mean_from_aux`.

- `aux_sd` If angular=FALSE and X is input, the standard deviation of X
  for the entire excursion. Values for each phase are also provided
  individually in columns `to_aux_sd`, `dest_aux_sd`, and `from_aux_sd`.

## Details

In addition to the maximum excursion and duration, `dive_stats` divides
each excursion into three phases: "to" (descent for dives, ascent for
flights), "from" (ascent for dives, descent for flights), and
"destination". The "destination" (bottom for dives and top for flights)
phase of the excursion is identified using a "proportion of maximum
depth/altitude" method, whereby for example the bottom phase of a dive
lasts from the first to the last time the depth exceeds a stated
proportion of the maximum depth. Average vertical velocity is computed
for the to and from phases using a simple method: total depth/altitude
change divided by total time. If an angular data variable is also
supplied (for example, pitch, roll or heading), then the circular mean
(computed via
[`circ.mean`](https://rdrr.io/pkg/CircStats/man/circ.mean.html)) and
variance (computed via
[`circ.disp`](https://rdrr.io/pkg/CircStats/man/circ.disp.html) and
reporting the `var` output) are also computed for each dive phase and
the dive as a whole.

## See also

[`find_dives`](https://animaltags.github.io/tagtools_r/reference/find_dives.md)
