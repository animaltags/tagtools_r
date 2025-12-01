# Vectors and scalars versus structures

Welcome to the `vectors-vs-structures` vignette! Thanks for taking some
time to get to know our package; we hope it is useful to you. In this
quick vignette, you will distinguish between vectors and structures.

*Estimated time for this practical: 15 minutes.*

These vignettes assume that you have R/Rstudio installed on your
machine, some basic experience working with them, and can execute
provided code, making some user-specific changes along the way (e.g. to
help R find a file you downloaded). We will provide you with quite a few
lines. To boost your own learning, you would do well to try and write
them before opening what we give, using this just to check your work.

## Setup: write “cats_test_raw.nc”, and load it as a large list, `MN`

For this vignette we will use the NetCDF file `cats_test_raw.nc`. If you
want to run this example, download the “cats_test_raw.nc” file from
<https://github.com/animaltags/tagtools_data> and change the file path
to match where you’ve saved the files

``` r
library(tagtools)
cats_file_path <- "nc_files/cats_test_raw.nc"
MN <- load_nc(cats_file_path)
```

This will write the file “cats_test_raw.nc”, and load it into your
current R session as a list object called `MN`. Looks good? Good work!
Confused? See the vignette `load-tag-data`.

## Sensor data structures & extracting data vectors

Having caught up, we return to sensor data structures. Like `$info`:

``` r
MN$info
```

Show/Hide Results

    #> $creation_date
    #> [1] "2023-06-14 12:58:11.202458"
    #> 
    #> $depid
    #> [1] "cats_test_raw"
    #> 
    #> $data_source
    #> [1] "20160730-091117-Froback-11-part.csv"
    #> 
    #> $data_nfiles
    #> [1] "1"
    #> 
    #> $data_format
    #> [1] "csv"
    #> 
    #> $device_make
    #> [1] "CATS"
    #> 
    #> $device_type
    #> [1] "Archival"
    #> 
    #> $dephist_device_tzone
    #> [1] "0"
    #> 
    #> $dephist_device_regset
    #> [1] "dd-mm-yyyy HH:MM:SS"
    #> 
    #> $dephist_device_datetime_start
    #> [1] "2016-07-27 16:29:38"
    #> 
    #> $sensors_list
    #> [1] "3 axis Accelerometer,3 axis Magnetometer,3 axis Gyroscope,Temperature,Pressure,Light level,"

these are structures that contain both the data and some information
about it. Typing any of the variable names followed by enter will show
what is in it. For example, try this for Acceleration:

``` r
str(MN$A, max.level = 1) # display the STRucture of MN$A
#> List of 19
#>  $ data              : num [1:720000, 1:3] -2.42 -2.4 -2.35 -2.3 -2.2 ...
#>  $ sampling          : chr "regular"
#>  $ sampling_rate     : num 400
#>  $ sampling_rate_unit: chr "Hz"
#>  $ depid             : chr "cats_test_raw"
#>  $ creation_date     : chr "2023-06-14 12:58:08.013504"
#>  $ history           : chr "read_cats"
#>  $ type              : chr "A"
#>  $ full_name         : chr "Acceleration"
#>  $ description       : chr "triaxial acceleration"
#>  $ unit              : chr "m/s2"
#>  $ unit_name         : chr "metres per second squared"
#>  $ unit_label        : chr "m/s^2"
#>  $ start_offset      : num 0
#>  $ start_offset_units: chr "second"
#>  $ column_name       : chr "x,y,z"
#>  $ frame             : chr "tag"
#>  $ axes              : chr "FRU"
#>  $ files             : chr "20160730-091117-Froback-11-part.csv"
```

In Matlab, Octave and R it will look a little different, but will have
all the same information. Specifically, we are after the variable `A`,
which is contained within the large list MN. `MN$A` is how we access it,
in R.

The results tell you that the acceleration data were sampled at a
nominal rate of 32 Hz (32 samples per second) and are in units of meters
per second squared. If you are more familiar with data vectors than with
structures, you can easily get the data out of the structure, and into
its own standalone object, using:

``` r
A <- MN$A$data
sampling_rate <- MN$A$sampling_rate
str(A, max.level = 1)
sampling_rate
```

Show/Hide Results

    #> [1] "Results for str(A, max.level = 1):"
    #> [1] "----------------------------------"
    #>  num [1:720000, 1:3] -2.42 -2.4 -2.35 -2.3 -2.2 ...
    #> [1] "----------------------------------"
    #> [1] "Result for sampling_rate:"
    #> [1] "----------------------------------"
    #> [1] 400

This makes a matrix A with the acceleration data and a scalar
`sampling_rate` with the sampling rate, which in this case is 32. You
can also just keep everything stored in the list object, if you like (in
R there is not a clear reason to pull them out). If you do this, you’d
just have to keep typing `MN$A$data` and `MN$A$sampling_rate` instead of
`A` or `sampling_rate`. If you move forward to `plots-and-cropping`, you
will still use the animaltag list `MN`, pulling data directly from it.

## Review

You’ve learned to distinguish between vectors and structures, and go
from a structure to a vector (or scalar).

Nice work! You’re all set on `vectors-vs-structures`.

*If you’d like to keep working through these practicals,
`plots-and-cropping` is a logical continuation. It’s particularly nice
to learn to use the plotter that comes with tagtools, called
[`plott()`](https://animaltags.github.io/tagtools_r/reference/plott.md).*

``` r
vignette('plots-and-cropping', package = 'tagtools')
```

*Otherwise, you might consider jumping ahead to
`data-quality-error-correction`. In it, you’ll get started with the work
of fixing errors in data.*

``` r
vignette('data-quality-error-correction', package = 'tagtools')
```

------------------------------------------------------------------------

Animaltags tag tools online: <http://animaltags.org/>,
<https://github.com/animaltags/tagtools_r> (for latest beta source
code), <https://animaltags.github.io/tagtools_r/index.html> (vignettes
overview)
