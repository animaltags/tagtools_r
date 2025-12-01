# Save an item to a NetCDF or add one tag sensor or metadata variable to a NetCDF archive file.

Add one tag sensor or metadata variable to a NetCDF archive file. If the
archive file does not exist, it is created. The file is assumed to be in
the current working directory unless a pathname is added to the
beginning of fname.

## Usage

``` r
add_nc(file, D, vname)
```

## Arguments

- file:

  The name of the netCDF file to which to save. If the name does not
  include a .nc suffix, this will be added automatically.

- D:

  The sensor data or metadata list to be saved.

- vname:

  The name of the sensor data stream to be saved. Defaults to the entry
  "name" (or "full_name" if there is no "name") from the sensor or
  metadata list provided by the user (but an option to specify a name is
  provided to facilitate calling this function from `save_nc`).

## Value

no object is returned; this function adds an animaltag sensor data
structure to a animaltag-style netCDF file containing tag data.

## See also

[`save_nc`](https://animaltags.github.io/tagtools_r/reference/save_nc.md),
[`load_nc`](https://animaltags.github.io/tagtools_r/reference/load_nc.md)

## Examples

``` r
BW <- beaked_whale
add_nc("beaked_whale", njerk(BW$A), "Jerk")
```
