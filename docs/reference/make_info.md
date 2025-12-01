# Make an info structure with tag metadata

This function allows the user to generate a "skeleton" info structure
for a tag deployment, with some common pieces of metadata filled in.
Additional information can then be added manually or using a custom
script before saving this `info` as part of a netCDF file.

## Usage

``` r
make_info(depid, tagtype, species, owner)
```

## Arguments

- depid:

  Deployment id string for this tag record

- tagtype:

  String identifying the tag type, for example 'dtag', 'cats', 'mk10',
  ...

- species:

  (optional) 2-letter string with the first letters of the species
  binomial

- owner:

  (optional) String with initials of the tag data owner

## Value

A list containing metadata for a tag deployment. It's recommended to
name this output "info" and save it as part of a netCDF tag data archive
file (along with the tag sensor data).

## Examples

``` r
info <- make_info("d4_template", "dtag4", "zc", "sdr")
```
