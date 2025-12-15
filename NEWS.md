# tagtools 0.3.0
This release removes a dependency, speeds up filtering and decimation via C++, fixes several bugs.

## Breaking changes

* `m_dist()` documentation and inputs have undergone several corrections, including re-naming inputs for consistency and removing some inputs that were never actually used. If you used the function before with named inputs, it is likely you will need to update code to the new input argument names.

## New features

* Dependency on latex2exp package has been removed, as requested by CRAN Team.

* Updates to `read_cats()` now allow reading deployments with multiple csv files

* Convolution in  `dec_dc()` is now implemented via RCppArmadillo, which speeds it up a LOT.

* Dependence on the package signal has been replaced by gsignal, which should be faster for some important cases. 

* interactivity in `prh_predictor1()` and `prh_predictor2()` is now optional (toggle via a new input argument). 

## Bug fixes

* Ensure output of `norm2()` is a column vector, not a neither-row-nor-column vector

* Move in-file helper functions outside curly braces in `o2p()` and `read_cats()` to avert function not found errors

* Correct documentation for `m_dist()`

* Update `save_nc()` to accept dots input

* Allow `add_nc()` to work correctly even if sensor data stream name is not input 

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
