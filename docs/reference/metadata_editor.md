# Edits a html file from given csv.

Takes data from csv, and edits a default or given html to fill in data
from the csv. HTML must be tagmetadata.html or variations, csv should
only contain metadata of tag.

## Usage

``` r
metadata_editor(
  masterHTML = system.file("extdata", "tagmetadata.html", package = "tagtools"),
  csvfilename = system.file("extdata", "blank_template.csv", package = "tagtools")
)
```

## Arguments

- masterHTML:

  default masterHTML is located in the package, or can be changed
  according to user input.

- csvfilename:

  file name of csv to be used for editing the HTML

## Value

A "dynamic tagmetadata.html" which is the masterHTML with changes from
csv. This file is written to the current working directory, and also
opened for editing by the user.
