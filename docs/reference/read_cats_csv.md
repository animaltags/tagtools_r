# Read a CSV file with sensor data from a CATS tag

Read in data from a CATS tag deployment (stored in one or more .csv
files). This function is usable by itself but is more normally called by
[`read_cats`](https://animaltags.github.io/tagtools_r/reference/read_cats.md)
which handles metadata and creates a NetCDF file.

## Usage

``` r
read_cats_csv(fname, max_samps = Inf, skip_samps = 0)
```

## Arguments

- fname:

  is the file name of the CATS CSV file(s) including a full or relative
  path. The .csv suffix is optional.

- max_samps:

  is optional and is used to limit reading to a maximum number of
  samples (rows) per sensor. This is useful to read in a part of a very
  large file for testing. If max_samps is not given, the entire file is
  read.

- skip_samps:

  Number of lines of data to skip (excluding header) before starting to
  read in data. Defaults to 0 (start at the beginning of the file), but
  could be used to read in a part of a file, or to read in and process a
  large file in chunks.

## Value

A tibble data frame containing the data read from the file. The column
names are taken from the first line of the CSV file and include units
and axis. Some columns may be empty (if for example, a tag did not
record data from a certain sensor type).

## Note

CATS csv files can be extremely large; perhaps too large to read the
entire file into memory at once and work with it.
