# Convert between dates and Julian day numbers.

This function is used to convert between dates and Julian day numbers.
There are three different input arrangements, each of which returns a
different output. For a description of the different input arrangements,
see below.

## Usage

``` r
julian_day(y = NULL, m = NULL, d = NULL)
```

## Arguments

- y:

  A single year or vector of years

- m:

  A single month or vector of months

- d:

  A single day or vector of days

## Value

See the description section for details on the return.

## Details

Possible input combinations: (n \<- julian_day()) returns the Julian day
number for today. (n = julian_day(y,d)) where y is a single year or a
vector of years and d is a single day number or a vector of daynumbers,
returns the date vector \[year,month,day\] for each year, day pair. (n =
julian_day(y,m,d)) where y is a single year or a vector of years, m is a
single month or vector of months, and d is a single month day or a
vector of month days, returns the Julian day number for each year,
month, day.

## Examples

``` r
julian_day(y = 2016, d = 12, m = 10)
#> [1] 286
julian_day(y = 2016, 286)
#> [1] "2016-10-12"
```
