# Convolution (in C++)

Performs 1D convolution of two vectors in C++ via FFT.

## Usage

``` r
conv_cpp(a, b, shape = "full")
```

## Arguments

- a:

  The first numeric vector.

- b:

  The second numeric vector.

- shape:

  (optional) The shape of the output: "full", "same", or "valid".
  Default is "full," matching
  [`conv`](https://rdrr.io/pkg/signal/man/conv.html) and the Armadillo
  default, which means that the output array is `length(a)` +
  `length(b)` - 1.

## Value

A numeric vector containing the convolution result.
