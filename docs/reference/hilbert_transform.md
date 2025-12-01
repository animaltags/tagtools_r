# Return the Hilbert transform of a signal

This function is used to compute the Hilbert transform of a signal. It
is based on function HilbertTransform() from (defunct) package hht,
which was modified from the EMD package by Donghoh Kim and Hee-Seok Oh
(http://dasan.sejong.ac.kr/~dhkim/software.emd.html)

## Usage

``` r
hilbert_transform(x)
```

## Arguments

- x:

  The signal vector to be buffered

## Value

The "analytic signal," in other words the Hilbert transform of the input
signal x

## Examples

``` r
timez <- seq(from = 0, by = 1/1024, to = 1)
x <- sin(2*pi*60*timez)
y <- hilbert_transform(x)
```
