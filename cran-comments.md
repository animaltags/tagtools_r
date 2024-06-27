## R CMD check results

0 errors | 0 warnings | 0 notes

## New changes

* the plott() function has been updated to produce graphs using the plotly package (for interactive graphs) and ggplot2 (for static ones) rather than base R. The new function plott_base() retains the original functionality of the old plott() function (with base R graphics).

* the read_cats() function has been updated to fix a bug: triaxial sensor data is now converted from the CATs standard right-handed NED orientation to the left-handed NEU one expected by other animaltags tools. The function also now deals better with the range of column names present in older and news CATs csv data files.

* The `col_line()` function has been removed because its purpose is easily accomplished with ggplot2 (or other) graphics.

## Test environments

* Checked on Mac OS (Sonoma 14.5) and Windows

## Dependencies

* Dependencies on ggplot2 and plotly have been added
