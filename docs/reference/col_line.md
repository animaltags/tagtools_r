# Plot coloured line(s) in 2 dimensions

This function is used to plot two dimensional lines with each individual
line possessing a different color.

## Usage

``` r
col_line(x, y, color, ...)
```

## Arguments

- x:

  A vector or matrix of values to display on the horizontal axis.

- y:

  A vector or matrix of values to display on the vertical axis.

- color:

  A vector or matrix of values representing the colour to draw at each
  point.

- ...:

  Additional inputs for plot()

## Value

a graph with a colored line

## Note

x, y and c must all be the same size. If x, y, and c are matrices, one
line is drawn for each column. The color axis will by default span the
range of values in c, i.e., caxis will be c(min(min(c)), max(max(c))).
This can be changed by calling caxis after colline.
