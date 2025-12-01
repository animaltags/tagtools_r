# Correct a depth or altitude profile

This function is used to correct a depth or altitude profile for offsets
caused by miscalibration and temperature. This function finds minima in
the dive/altitude profile that are consistent with surfacing/landing. It
uses the depth/height at these points to fit a temperature regression.

## Usage

``` r
fix_pressure(p, t, sampling_rate, maxp = NULL)
```

## Arguments

- p:

  A sensor list or vector of depth/altitude in meters

- t:

  A sensor list or vector of temperature in degrees Celsius

- sampling_rate:

  The sampling_rate of p and t in Hz. This is only needed if p and t are
  not sensor lists. The depth and temperature must both have the same
  sampling rate (use \`decdc\` if needed to achieve this).

- maxp:

  The maximum depth or altitude reading in the pressure data for which
  the animal could actually be at the surface. This is a rough
  measurement of the potential error in the pressure data. The unit is
  meters. Start with a small value, e.g., 2m and rerun fix_pressure with
  a larger value if there are still obvious temperature-related errors
  in the resulting depth/altitude profile.

## Value

A list with 2 elements:

- **p:** A sensor list or vector of corrected depth/altitude
  measurements at the same sampling rate as the input data. If the input
  is a sensor list, the output will also be.

- **pc:** A list containing the pressure offset and temperature
  correction coefficients. It has fields: pc\$tref which is the
  temperature compensation polynomial. This is used within the function
  to correct pressure as follows: p + stats::polyval(pc\$tcomp, t -
  pc\$tref).

## Note

This function makes a number of assumptions about the depth/altitude
data and about the behaviour of animals: First, the depth data should
have few incorrect outlier (negative) values that fall well beyond the
surface. These can be reduced using median_filter.m before calling
fix_depth. Second, the animal is assumed to be near the surface at least
2
