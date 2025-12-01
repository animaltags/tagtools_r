# Save a tag dataset to a netCDF file.

This function saves a tag dataset to a netCDF file (this is an archival
file format supported by the tagtools package and suitable for
submission to online data archives).

## Usage

``` r
save_nc(file, X, ...)
```

## Arguments

- file:

  The name of the data and metadata file to be written. If `file` does
  not include a .nc suffix, this will be added automatically.

- X:

  An `animaltag` object, or a list of tag sensor and/or metadata lists.
  Alternatively, sensor and metadata lists may be input as multiple
  separate unnamed inputs. Only these kind of variables can be saved in
  a NetCDF file because the supporting information in these structures
  is needed to describe the contents of the file. For non-archive and
  non-portable storage of variables, consider using
  [`save`](https://rdrr.io/r/base/save.html) or various functions to
  write data to text files.

- ...:

  Additional sensor or metadata lists, if user has not bundled them all
  into a list already but is providing individual structures.

## Value

no return; saves a dataset to an nc file

## Details

Warning: this will overwrite any previous NetCDF file with the same
name. The file is assumed to be in the current working directory unless
`file` includes file path information.

## Examples

``` r
BW <- beaked_whale
save_nc("beaked_whale_test", BW)
```
