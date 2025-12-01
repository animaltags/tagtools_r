# Tag orientation: From tag frame to sperm whale frame

Welcome to the tag orientation vignette!

Thanks for taking some time to work with our package. We hope you’re
having a great day!

This vignette deals with estimating and correcting the orientation of a
tag on an animal. It will use tag data that are not aligned to the
animal’s body axes, e.g., because the tag was applied to a free-moving
whale. Your task will be to align the data to the animal’s body axes.

Additionally, be careful when copy-pasting special characters such as
\_underscores\_ and ‘quotes’. If you get an error, one thing to check is
that you have just a single, simple underscore, and `'straight quotes'`,
whether `'single'` or `"double"` (rather than “smart quotes”).

*Estimated time for this vignette: 25 minutes.*

These practicals all assume that you have R/Rstudio installed on your
machine, some basic experience working with them, and can execute
provided code, making some user-specific changes along the way (e.g. to
help R find a file you downloaded). We will provide you with quite a few
lines. To boost your own learning, you would do well to try and write
them before opening what we give, using this just to check your work.

## From tag frame to sperm whale frame

For tags placed on free-swimming animals, the tag axes will not
generally coincide with the animal’s axes. The tag `A` and `M` data will
therefore tell you how the tag, not the animal, is oriented. This can be
corrected if you know the orientation of the tag on the animal, i.e.,
the pitch, roll and heading of the tag when the animal is horizontal and
pointing north. Three steps are needed to do this: First the tag
orientation on the whale is inferred by looking at accelerometer values
when the animal is near the surface. The orientation may change if the
tag moves or slides during a deployment and so we make an ‘orientation
table’ that describes the sequence of orientations of the tag. This
table is then used to convert tag frame measurements into animal frame.

### Estimate the tag orientations on the whale

Use
[`load_nc()`](https://animaltags.github.io/tagtools_r/reference/load_nc.md)
to read `testset5.nc`, which is from a pilot whale in the Canary
Islands. Write it to the object `sw` to follow along with the code in
this vignette. If you want to run this example, download the
“testset5.nc” file from <https://github.com/animaltags/tagtools_data>
and change the file path to match where you’ve saved the files

``` r
library(tagtools)
sw_file_path <- "nc_files/testset5.nc"
sw <- load_nc(sw_file_path)
```

We will use
[`prh_predictor1()`](https://animaltags.github.io/tagtools_r/reference/prh_predictor1.md)
to infer the tag-to-animal orientation. This function is for animals
like pilot whales that log (more or less) at the surface before diving
steeply. It assumes two things:

- that the animal rests horizontally at the surface (at least on
  average).
- that it dives steeply away from the surface without rolling, at least
  initially.

If these are not good assumptions for your animal,
[`prh_predictor2()`](https://animaltags.github.io/tagtools_r/reference/prh_predictor2.md)
may work. It is suitable for whales such as beaked whales that make
short dives between breaths at the surface. Both tools try to predict
the tag orientation on the whale parameterized by its pitch, roll and
yaw (which we shall call `p0`, `r0`, and `h0`). To use
[`prh_predictor1()`](https://animaltags.github.io/tagtools_r/reference/prh_predictor1.md),
we need to define a minimum dive depth to analyse—let’s say 400 m. Then
call the tool like this:

``` r
PRH = prh_predictor1(sw$P, sw$A, TH = 400)
```

A graphical interface will open showing the dive profile (top), the
estimated tag-to-animal orientations throughout the deployment (middle
panel), and the quality of these estimates (bottom panel). In the middle
panel, the colored stars show the `p0`, `r0` and `h0` estimates at the
start of each dive.

A quality measure of \<0.05 in the bottom panel indicates that the data
fit well with the assumptions of the method.

If the tag does not move on the animal from dive to dive, then `p0`,
`r0` and `h0` should be fairly constant across dives. If they are not
constant it can be due to a sudden change (e.g., the tag gets hit) or a
slow movement of the tag on the animal. In this case it seems that there
is a change in orientation between dives 6 and 7.

The function automatically picks segments of data to analyse, and you
should check that it did this well, especially if the ‘quality’ number
is \> 0.05. The third dive has a slightly bad quality: type `e` on the
main figure to “edit” and then click on one of the third dive
orientation points. *Note that prompts will appear at the very top of
the figure window(s), which will update based on the most recent user
actions.*

A new plot will be drawn in the second figure window (pull it up if it
does not automatically come to the front) in which the top panel shows
the acceleration data in the tag frame for the current dive edge. Blue,
orange or red (depending on your Matlab/Octave version) and yellow
represent the x, y and z axes, respectively.

The bottom panel shows the acceleration data after correction using the
`p0`, `r0` and `h0` estimates printed above the panel.

Two black rectangles show the data segments that have been chosen for
analysis and these may need to be moved:

- The left-hand or ‘surface’ segment should be positioned just before
  the start of the dive.
- The right-hand or ‘dive’ segment should be just after the start of the
  dive.
- The surface segment should cover an interval of consistent surfacing
  behaviour (either logging or shallow diving).
- The dive segment should contain only roll-free steep diving, i.e., the
  blue and red lines (the x- and y-axes) should be close to -9.8 m/s2
  and 0, respectively, in the lower panel.

The segment edges are numbered 1 to 4 from the left. To change a
segment, hover the mouse over the plot and type the relevant number,
then click the mouse where you want the newly-drawn box edge to be. The
segment will move and the `p0`, `r0`, `h0` and quality will be
re-calculated. When you are satisfied with the estimate quality, type q
and then return to the first figure.

### Produce the orientation table

We need to create an orientation table (OTAB) that summarizes the tag
orientation on the animal as a function of time. Each row of the OTAB
matrix describes the orientation of the tag on the animal over a time
interval. If there are two moves, the OTAB will have three lines: the
initial orientation and the orientation after each move.

Each row defines how the tag is oriented on the animal and has columns:
`t1`, `t2`, `p0`, `r0`, `h0`. `t1` and `t2` are the start and end times
of the move (in seconds-since-tag-on), and `p0`, `r0`, `h0` are the new
orientation Euler angles after the move (in radians).

The initial orientation (i.e., the first row in the OTAB) always has
`t1` = `t2` = 0. For subsequent moves, if the move is instantaneous, use
`t2` = `t1`. If the move is gradual, set `t1` to the time at which the
movement appears to start and set `t2` to the time when the move appears
to be complete.

Making the OTAB is a bit of a black art.
[`prh_predictor1()`](https://animaltags.github.io/tagtools_r/reference/prh_predictor1.md)
is not infinitely precise, and an apparent change of less than 10
degrees is probably not worth worrying about.

We are going to assume there is just one move (i.e., during the 6th
dive) in our pilot whale data. Position the mouse over the blue (`p0`),
red or orange (`r0`), and yellow (`h0`) lines before the move and click
the left button on each to get the angle (in degrees). Make the first
OTAB line:

``` r
otab1 <- matrix(data = c(0,0, c(p0, r0 , h0)*pi/180), nrow = 1)
```

where `p0`, `r0` and `h0` are your readings. The pi/180 converts these
into radians.

For the second OTAB line, read off the `p0`, `r0` and `h0` values after
the move and decide when the move actually happens (let’s say it is a
sudden move at the end of the dive, e.g., at 10100 seconds). Then:

``` r
otab2 <- matrix(data = c(10100, 10100, c(p0, r0, h0)*pi/180), nrow = 1)
```

Finally, type ‘q’ to quit prh_predictor1. Now you can make the OTAB
matrix:

``` r
OTAB <- rbind(otab1, 
              otab2)
```

### Tag frame to animal frame

Use your OTAB to convert tag frame measurements (`A` or `M`) to animal
frame:

``` r
Aa <- tag2animal(sw$A, OTAB = OTAB) 
```

For this particular dataset, only `A` is provided, but in case you want
to also convert tag frame magnetometer measurements to animal frame, you
could do so in a similar way, using code of the form:

``` r
Ma <- tag2animal(sw$M, OTAB = OTAB)
```

`Aa` should now contain acceleration data like what would have been
recorded by a tag aligned with the animal’s axes, *i.e.*, in the animal
frame. Compute pitch and roll from `Aa` and plot them to check they make
sense:

``` r
pr <- a2pr(Aa)
pitch <- pr$p
roll <- pr$r
rm(pr)

plott(X = list(depth = P, pitch = pitch*180/pi, roll = roll*180/pi),
      fsx = Aa.sampling_rate, interactive = TRUE)
```

The `pitch` and `roll` vectors are in radians (hence the \*180/pi to
covert to degrees) and have the same sampling rate as `Aa`. Zoom in to
check if the animal is in a realistic orientation when it is at the
surface, or during an ascent or descent.

When you are comfortable that the tag is well aligned to the animal
frame, you can add the corrected data to the archive file:

``` r
add_nc('testset5_animal_frame', Aa)
```

There is no need to also save `pitch` and `roll` because these can be
easily re-computed from `Aa`. The archive file should only contain
source data or data that has been corrected. The orientation correction
steps and the `OTAB` are stored in `Aa`, and so this information is
saved automatically in your archive file. Nice work!

#### Extra quality check

If you have time and want something extra, do the following check.

The code below will compute the smoothed pitch angle; 12 is the
smoothing parameter. Signal components above 1/12 of the Nyquist
frequency are filtered out.

``` r
pitch_s <- a2pr(tagtools::smooth(Aa.data, 12))
```

Now find the whale’s vertical speed in m/sec:

``` r
v <- depth_rate(sw$P)        
```

And plot it. Feel free to convert to your favorite graphics system - I
love
[ggformula](https://cran.r-project.org/web/packages/ggformula/vignettes/ggformula.html)…

``` r
plot(v, pitch_s*180/pi, type = 'p', pch = 16,
     ylab = 'smooth pitch (degrees)',
     xlab = 'speed (m/sec)')   
```

If `Aa` is done right, there should be a fairly good negative
correlation between depth rate and pitch. There will be some outliers
because we didn’t put the tag orientation change in *quite* the right
place. If you feel like making some fine adjustments, improve your OTAB
and perform this check again. With your own data, you will likely spend
*a lot* of time making these small adjustments.

## Review

You’ve learned how to estimate tag orientation over time, as well as how
to correct for it when these estimates are problematic.

Congrats! You made it through this vignette.

*If you’d like to continue working through these vignettes, consider
`acceleration-filtering`. In it, you’ll look at acceleration data, and
consider quick changes in acceleration (high-frequency acceleration)
versus slower changes (low-frequency acceleration) to interpret an
animal’s behavior.*

``` r
vignette('acceleration-filtering')
```

------------------------------------------------------------------------

Animaltags tag tools online: <http://animaltags.org/>,
<https://github.com/animaltags/tagtools_r> (for latest beta source
code), <https://animaltags.github.io/tagtools_r/index.html> (vignettes
overview)
