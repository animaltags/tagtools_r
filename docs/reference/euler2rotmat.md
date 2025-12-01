# Make a rotation (or direction cosine) matrix

This function is used to make a rotation (or direction cosine) matrix
out of sets of Euler angles, pitch, roll, and heading.

## Usage

``` r
euler2rotmat(p, r, h)
```

## Arguments

- p:

  The pitch angle in radians.

- r:

  The roll angle in radians.

- h:

  The heading or yaw angle in radians.

## Value

One or more 3x3 rotation matrices. If p, r, and h are all scalars, Q is
a 3x3 matrix, Q = H

## Examples

``` r
vec1 <- matrix(c(1:10), nrow = 10)
vec2 <- matrix(c(11:20), nrow = 10)
vec3 <- matrix(c(21:30), nrow = 10)
Q <- euler2rotmat(p = vec1, r = vec2, h = vec3)
```
