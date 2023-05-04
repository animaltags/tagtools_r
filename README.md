# tagtools_r

This repository contains an R package with the R version of the animaltag tool kit. 

Documentation and vignettes are available at: https://animaltags.github.io/tagtools_r/

High-resolution movement-sensor tags typically include accelerometers 
to measure body posture and sudden movements or changes in speed, 
magnetometers to measure direction of travel, and pressure sensors
to measure dive depth in aquatic or marine animals. 
A subset of tags include sensors for speed, turning rate (gyroscopes), and sound. 
This package provides software tools to facilitate calibration, processing, 
and analysis of such data. Tools are provided for: Data Import/export, 
Calibration (from raw data to calibrated data in scientific units), 
Visualization (e.g., time-series plots, multiple events overlaid, long-term spectral averages), 
Data Processing (e.g., event detection, derived metrics like jerk and 
dynamic acceleration, Dive detection and dive parameter calculation, 
Integrating movement data with other sensors eg acoustic or camera, 
integrating position data from onboard GPS, visual observations, etc. with movement data, 
and statistical Analysis (e.g., track reconstruction, Mahalanobis distance analysis).

This material is based upon research supported by the United States Office of Naval Research under Award Number N00014-16-1-3089.

