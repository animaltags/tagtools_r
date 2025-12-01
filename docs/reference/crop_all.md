# Reduce the time span of a dataset

This function is used to reduce the time span of a dataset by cropping
out any data that falls before and after two time cues.

## Usage

``` r
crop_all(tcues, X)
```

## Arguments

- tcues:

  A two-element vector containing the start and end time cue in seconds
  of the data segment to keep, i.e., tcues = c(start_time, end_time).

- X:

  A sensor list or a set of sensor lists (e.g., from load_nc).

## Value

A sensor list or set of sensor lists containing the cropped data
segment. The output data have the same units, frame and sampling
characteristics as the input. The list may have many sublists which are
additional sensor structures as required to match the input.

## Details

Possible input combinations: crop_all(X) if X is a sensor list or set of
sensor lists, crop_all(tcues, X, Y, ...) if X, Y, ... are sensor lists.

## Examples

``` r
d <- find_dives(beaked_whale$P,300) 
         X <- crop_all(c(d$start[1], d$end[1]), beaked_whale)  #crop all data to 1st dive
         plott_base(X = list(X$P, X$A), r = c(1, 0), panel_labels = c('Depth', 'Acc'))
```
