# tagtools 0.2.0

This release adds several new features and fixes several bugs.

## Breaking changes

* `plott()` now generates interactive plots via the plotly package, and static ones via ggplot2. If you prefer base R graphics, the old implementation is still present at `plott_base()`.

* Changes to `read_cats()` mean it now changes all triaxial sensor data from the CATs standard right-handed NED axis orientation to the left-handed NEU one employed by the animaltags tagtools. If you employed custom code to make this change after reading in CATs data, the custom code will no longer be needed.

* The `col_line()` function has been removed because its purpose is easily accomplished with ggplot2 (or other) graphics.

## New features

* `plott()` now generates interactive plots via the plotly package, and static ones via ggplot2. If you prefer base R graphics, the old implementation is still present at `plott_base()`. `plott()` also now has an option to draw the plot, or instead return a list of graphics objects (one for each panel). This is helpful for users who would like to amend individual panels (for example, overlaying event times on a dive or flight profile).

## Bug fixes

* `read_cats()` has been updated to deal better with the column names present in more recent CATs csv data files. It has also been amended to change all triaxial sensor data from the CATs standard right-handed NED axis orientation to the left-handed NEU one employed by the animaltags tagtools.

# tagtools 0.1.0

* First CRAN release

* Added a `NEWS.md` file to track changes to the package.
