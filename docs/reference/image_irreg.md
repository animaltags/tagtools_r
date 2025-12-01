# Plot an image with an irregular grid.

This function is used to plot an image with an irregular grid. This is
useful for plotting matrix data (i.e., sampled data that is a function
of two parameters) in which one or both of the sampling schemes is not
regularly spaced. image_irreg plots R(i,j) as a coloured patch centered
on x(i), y(j) and with dimension determined by x\[i\] - x\[i-1\] and
y\[i\] - y\[i-1\].

## Usage

``` r
image_irreg(x, y, R)
```

## Arguments

- x:

  is a vector with the horizontal axis coordinates of each value in R.

- y:

  is a vector with the vertical axis coordinates of each value in R.

- R:

  is a matrix of measurements to display. The values in R are converted
  to colours in the current colormap and caxis. R must be length(x) by
  length(y). Use NaN to have a patch not display.

## Value

an image plot on an irregular grid
