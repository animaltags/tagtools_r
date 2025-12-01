# Carry out a rotation randomization test.

Carry out a rotation test (as applied in Miller et al. 2004 and detailed
in DeRuiter and Solow 2008). This test is a variation on standard
randomization or permutation tests that is appropriate for time-series
of non-independent events (for example, time series of behavioral events
that tend to occur in clusters).

## Usage

``` r
rotation_test(
  event_times,
  exp_period,
  full_period = range(event_times, na.rm = TRUE),
  n_rot = 10000,
  ts_fun = length,
  skip_sort = FALSE,
  conf_level = 0.95,
  return_rot_stats = FALSE,
  ...
)
```

## Arguments

- event_times:

  A vector of the times of events. Times can be given in any format. If
  `event_times` should not be sorted prior to analysis (for example, if
  times are given in hours of the day and the times in the dataset span
  several days), be sure to specify `skip_sort=TRUE`.

- exp_period:

  A two-column vector, matrix, or data frame specifying the start and
  end times of the "experimental" period for the test. If a matrix or
  data frame is provided, one column should be start time(s) and the
  other end time(s). Note that all data that falls into any experimental
  period will be concatenated and passed to `ts_fun`. If finer control
  is desired, consider writing your own test using the underlying
  function `rotate_data`.

- full_period:

  A length two vector giving the start and end times of the full period
  during which events in event_times might have occurred. If missing,
  default is range(`event_times`).

- n_rot:

  Number of rotations (randomizations) to carry out. Default is
  `n_rot=10000`.

- ts_fun:

  A function to compute the test statistic. Input provided to this
  function will be the times of events that occur during the
  "experimental" period. The default function is `length` - in other
  words, the default test statistic is the number of events that happen
  during the experimental period.

- skip_sort:

  Logical. Should times be sorted in ascending order? Default is
  `skip_sort=FALSE`.

- conf_level:

  Confidence level to be used for the bootstrap CI calculation,
  specified as a proportion. (default is `conf_level=0.95`, or 95%
  confidence.)

- return_rot_stats:

  Logical. Should output include the test statistics computed for each
  rotation of the data? Default is `return_rot_stats=FALSE`.

- ...:

  Additional inputs to be passed to `ts_fun`

## Value

A list containing the following components:

- **result**, A one-row data frame with rows:

  - **statistic:** Test statistic (from original data)

  - **p_value:** P-value of the test (2-sided)

  - **n_rot:** Number of rotations

  - **CI_low:** Lower bound on rotation-resampling percentile-based
    confidence interval

  - **CI_up:** Upper bound on rotation-resampling percentile-based
    confidence interval

  - **conf_level:** Confidence level, as a proportion

- **rot_stats** (If `return_rot_stats` is TRUE), a vector of `n_rot`
  statistics from the rotated datasets

## Details

This implementation of the rotation test compares a test statistic (some
summary of an "experimental" time-period) to its expected value during
non-experimental periods. Instead of resampling random subsets of
observations from the original dataset, the rotation test samples many
contiguous blocks from the original data, each the same duration as the
experimental period. The summary statistic, computed for these "rotated"
samples, provides a distribution to which the test statistic from the
data can be compared.

## References

Miller, P. J. O., Shapiro, A. D., Tyack, P. L. and Solow, A. R. (2004).
Call-type matching in vocal exchanges of free-ranging resident killer
whales, Orcinus orca. Anim. Behav. 67, 1099–1107.

DeRuiter, S. L. and Solow, A. R. (2008). A rotation test for behavioural
point-process data. Anim. Behav. 76, 1103–1452.

## See also

Advanced users seeking more flexibility may want to use the underlying
function
[`rotate_data`](https://animaltags.github.io/tagtools_r/reference/rotate_data.md)
to carry out customized rotation resampling.
[`rotate_data`](https://animaltags.github.io/tagtools_r/reference/rotate_data.md)
generates one rotated dataset from `event_times` and `exp_period`.

## Examples

``` r
r <- rotation_test(
  event_times =
    2000 * runif(500),
  exp_period = c(100, 200),
  return_rot_stats = TRUE, ts_fun = mean
)
```
