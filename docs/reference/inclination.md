# Estimate the inclination angle

This function is used to estimate the local magnetic field vector
inclination angle directly from acceleration and magnetic field
measurements.

## Usage

``` r
inclination(A, M, fc = NULL)
```

## Arguments

- A:

  The accelerometer data structure or signal matrix, A = \[ax,ay,az\] in
  any consistent unit (e.g., in g or m/s2). A can be in any frame.

- M:

  The magnetometer data structure or signal matrix, M = \[mx,my,mz\] in
  any consistent unit (e.g., in uT or Gauss). M must be in the same
  frame as A.

- fc:

  (optional) The cut-off frequency of a low-pass filter to apply to A
  and M before computing the inclination angle. The filter cut-off
  frequency is with respect to 1=Nyquist frequency. Filtering adds no
  group delay. If fc is not specified, no filtering is performed.

## Value

The magnetic field inclination angle in radians.

## Note

Output sampling rate is the same as the input sampling rate.

Frame: This function assumes a \[north,east,up\] navigation frame and a
\[forward,right,up\] local frame. In these frames, the magnetic field
vector has a positive inclination angle when it points below the
horizon. Other frames can be used as long as A and M are in the same
frame however the interpretation of incl will differ accordingly.

## Examples

``` r
A <- matrix(c(1, -0.5, 0.1, 0.8, -0.2, 0.6, 0.5, -0.9, -0.7),
  byrow = TRUE, nrow = 3, ncol = 3
)
M <- matrix(c(1.3, -0.25, 0.16, 0.78, -0.3, 0.5, 0.5, -0.49, -0.6),
  byrow = TRUE, nrow = 3, ncol = 3
)
incl <- inclination(A, M)
```
