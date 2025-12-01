# Read tag metadata from csv

Read a CSV metadata file and convert it into a metadata list. A metadata
file is a comma-separated text file (.csv) containing a line for each
metadata entry. The first comma-separated field in each line is the name
of the entry. The last field in each line contains the value to be
assigned to this metadata entry. The value can be a string or number but
is always saved as a string in the structure - it is up to downstream
users of the metadata to parse/decode the entries.

## Usage

``` r
csv2struct(fname)
```

## Arguments

- fname:

  Name of the text file to be read. If no file extension is provided,
  '.csv' will be added automatically. If the file is not located in the
  current working directory, then `fname` must include the correct
  relative or absolute path.

## Value

a metadata list populated from `fname` (one list element per row in the
file). All list elements are stored as `"character"` class objects (even
if the field contains a number, a date, etc) - no attempt is made to
determine the most appropriate class for each item.

## Examples

``` r
hold <- system.file("extdata","metadata_example.csv", package = "tagtools", mustWork = TRUE)
S <- csv2struct(hold)
```
