# Add colored line segments to a plot

This function adds colored line segments to an existing plot. The line
is plotted at points specified by inputs x and y, and colored according
to factor input z (with one color for each level of z).

## Usage

``` r
cline(x, y, z, color_vector)
```

## Arguments

- x:

  x positions of points to be plotted

- y:

  y positions of points to be plotted

- z:

  a factor, the same length as x and y. Line segments in the resulting
  plot will be colored according to the levels of z.

- color_vector:

  a list of colors to use (length should match the number of levels in
  z).

## Value

adds colored lines to a graph

## Examples

``` r
cline(x=ChickWeight$Time, y=ChickWeight$weight, 
      z=as.factor(ChickWeight$Diet), 
      color_vector=c('black', 'grey20', 
                     'grey50', 'grey70'))
#> Error in plot.xy(xy.coords(x, y), type = type, ...): plot.new has not been called yet
```
