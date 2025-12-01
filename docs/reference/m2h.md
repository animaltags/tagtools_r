# Heading from accelerometer and magnetometer data

This function is used to compute the heading, field intensity, and the
inclination angle by gimballing the magnetic field measurement matrix
with the pitch and roll estimated from the accelerometer matrix.

## Usage

``` r
m2h(M, A, sampling_rate = NULL, fc = NULL)
```

## Arguments

- M:

  A sensor data structure or matrix, M = \[mx,my,mz\] in any consistent
  unit (e.g., in uT or Gauss) or magnetometer sensor list (e.g., from
  readtag.R).

- A:

  A sensor data structure or matrix with columns \[ax ay az\] or
  acceleration sensor list (e.g., from readtag.R). Acceleration can be
  in any consistent unit, e.g., g or m/s^2.

- sampling_rate:

  (optional) The sampling rate of the sensor data in Hz (samples per
  second). This is only needed if filtering is required. If `A` and `M`
  are sensor data lists, then sampling_rate is obtained from them.

- fc:

  (optional) The cut-off frequency of a low-pass filter to apply to A
  and M before computing heading. The filter cut-off frequency is with
  in Hertz. The filter length is 4\*sampling_rate/fc. Filtering adds no
  group delay. If fc is not specified, no filtering is performed.

## Value

A list with 3 elements:

- **h:** The heading in radians in the same frame as M. The heading is
  with respect to magnetic north (i.e., the north vector of the
  navigation frame) and so must be corrected for declination.

- **v:** The estimated magnetic field intensity in the same units as M.
  This is computed by taking the 2-norm of M, after filtering (if any
  filtering was specified).

- **incl:** The estimated magnetic field inclination angle (i.e., the
  angle with respect to the horizontal plane) in radians. By convention,
  a field vector pointing below the horizon has a positive inclination
  angle. See note in the function if using incl.

## Note

Output sampling rate is the same as the input sampling rate (i.e. h, v,
and incl are estimated with the same sampling rate as M and A and so are
each nx1 vectors).

Frame: This function assumes a \[north,east,up\] navigation frame and a
\[forward,right,up\] local frame. North and east are magnetic, not true.
In these frames a positive heading is a clockwise rotation around the
z-axis.

The heading is computed with respect to the frame of M and is the
magnetic heading NOT the true heading. M and A must have the same
sampling rate, frame, and number of rows.

## See also

[`a2pr`](https://animaltags.github.io/tagtools_r/reference/a2pr.md)

## Examples

``` r
m2h_out <- m2h(M = matrix(c(22, -24, 14), nrow = 1), 
                        A = matrix(c(-0.3, 0.52, 0.8), nrow = 1))
```
