# Predict the tag position on a diving animal from depth and acceleration data

Predict the tag position on a diving animal parameterized by p0, r0, and
h0, the canonical angles between the principal axes of the tag and the
animal. The tag orientation on the animal can change with time and this
function provides a way to estimate the orientation at the start and end
of each suitable dive. The function critically assumes that the animal
rests horizontally at the surface (at least on average) and dives
steeply away from the surface without an initial roll. If ascents are
processed, there must also be no roll in the last seconds of the
ascents. See prh_predictor2 for a method more suitable to animals that
make short dives between respirations. The function provides a graphical
interface showing the estimated tag-to-animal orientation throughout the
deployment. Follow the directions above the top panel of the figure to
edit or delete an orientation estimate.

## Usage

``` r
prh_predictor1(P, A, sampling_rate = NULL, TH = 100, DIR = "descent")
```

## Arguments

- P:

  is a dive depth vector or sensor structure with units of m H2O.

- A:

  is an acceleration matrix or sensor structure with columns ax, ay, and
  az. Acceleration can be in any consistent unit, e.g., g or m/s^2, and
  must have the same sampling rate as P.

- sampling_rate:

  is the sampling rate of the sensor data in Hz (samples per second).
  This is only needed if neither A nor M are sensor structures.

- TH:

  is an optional minimum dive depth threshold (default is 100m). Only
  the descents at the start of dives deeper than TH will be analysed
  (and the ascents at the end of dives deeper than TH if ALL is true).

- DIR:

  is an optional dive direction constraint. The default (DIR =
  'descent') is to only analyse descents as these tend to give better
  results. But if DIR = 'both', both descents and ascents are analysed.

## Value

PRH, a data frame with columns `cue` `p0`, `r0`, `h0`, and `q` with a
row for each dive edge analysed. `cue` is the time in
second-since-tag-start of the dive edge analysed. `p0`, `r0`, and `h0`
are the deduced tag orientation angles in radians. `q` is the quality
indicator with a low value (near 0, e.g., \<0.05) indicating that the
data fit more consistently with the assumptions of the method.

## See also

[prh_predictor2](https://animaltags.github.io/tagtools_r/reference/prh_predictor2.md),
[tag2animal](https://animaltags.github.io/tagtools_r/reference/tag2animal.md)
