# Calculate Mahalanobis distance

This function is used to calculate the Mahalanobis distance for a
multivariate time series.

## Usage

``` r
m_dist(
  data,
  sampling_rate = 1,
  smooth_dur = 0,
  overlap = 0,
  consec = FALSE,
  cum_sum = FALSE,
  bl_start = 0,
  bl_end = floor(nrow(data)/sampling_rate),
  bl_cov = FALSE
)
```

## Arguments

- data:

  A data frame or matrix with one row for each time point. Note that the
  Mahalanobis distance calculation should be carried out on continuous
  data only, so if your data contain logical, factor or character data,
  proceed at your own risk...errors (or at least meaningless results)
  will probably ensue.

- sampling_rate:

  The sampling rate in Hz (data should be regularly sampled). If not
  specified it will be assumed to be 1 Hz.

- smooth_dur:

  The length, in minutes, of the window to use for calculation of
  "comparison" values. If not specified or zero (the default), there
  will be no smoothing (a distance will be calculated for each data
  observation).

- overlap:

  The amount of overlap, in minutes, between consecutive "comparison"
  windows. `smooth_dur` - `overlap` will give the time resolution of the
  output distance time series. Default is 0, which means no overlap.
  Overlap will also be set to zero if `smooth_dur` is unspecified or
  zero.

- consec:

  Logical (default FALSE). If `consec` is TRUE, then the calculated
  distances are between \*consecutive windows\* of duration
  `smooth_dur`, sliding forward over the data set by a time step of
  (`smooth_dur` - `overlap`) minutes. Default is `consec` = FALSE, which
  means each output distance will be the distance between the current
  "comparison" window and the baseline window. If `consec` is TRUE,
  `bl_start` and `bl_end` inputs will be used \*only\* to define the
  period used to calculate the data covariance matrix.

- cum_sum:

  Logical (default FALSE). If `cum_sum` is TRUE, then output will be the
  cumulative sum of the calculated distances, rather than the distances
  themselves. Default is cum_sum = FALSE.

- bl_start:

  Start time (in seconds since start of the data set) of the baseline
  period. The mean data values for this period will be used as the
  'control' to which all "comparison" data points (or windows) will be
  compared. If not specified, it will be assumed to be 0 (start of
  record). If `consec` is TRUE, then adjacent windows will be compared,
  and this input will have no effect except to define the data used to
  compute the covariance matrix, if `bl_cov` is TRUE.

- bl_end:

  End time (in seconds since start of the data set) of the baseline
  period. If not specified, the entire data set will be used
  (baseline_end will be the last sampled time-point in the data set). If
  consec = TRUE, then adjacent windows will be compared, and this input
  will have no effect except to define the data used to compute the
  covariance matrix, if `bl_cov` is TRUE.

- bl_cov:

  Logical. If bl_cov is TRUE, then a covariance matrix using all data
  \*in the baseline period\* will be used for calculating the
  Mahalanobis distance. Default is bl_cov = FALSE, which uses \*all\*
  data (in the entire dataset) to compute the covariance matrix.

## Value

Data frame containing results: variable `seconds` is times (in seconds
since start of dataset) at which Mahalanobis distances are reported. If
a `smooth_dur` window was applied, then the reported times will be the
midpoint of each "comparison" window. Variable `dist` contains the
computed Mahalanobis distances.

## Examples

``` r
BW <- beaked_whale
m_dist_result <- m_dist(BW$A$data, BW$A$sampling_rate)
```
