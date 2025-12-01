# Draw time axis on plott plot.

This function is called by
[`plott`](https://animaltags.github.io/tagtools_r/reference/plott.md) to
add a time axis to a plot created by
[`plott`](https://animaltags.github.io/tagtools_r/reference/plott.md).
Users are unlikely to need to call the function directly.

## Usage

``` r
draw_axis(side = 1, x = NULL, date_time, last_panel)
```

## Arguments

- side:

  see [`axis`](https://rdrr.io/r/graphics/axis.html).

- x:

  A date-time or date object, or other types of objects that can be
  converted appropriately.

- date_time:

  Logical. Is the data being plotted date-time (POSIX) or time in
  seconds?

- last_panel:

  Logical. Is this the last panel (in other words, should x axis tick
  labels be drawn)?

## Value

a time axis on a graph
