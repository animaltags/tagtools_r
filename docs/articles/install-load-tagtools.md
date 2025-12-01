# Installing and loading tagtools

Welcome to this vignette! On behalf of the team behind tagtools, thanks
for taking some time to get to know this package. We hope it is useful
to you.

In this vignette you will complete the first important setup steps of
installing the package on your machine and making sure it is loaded in
your session.

*Estimated time for this vignette: 15 minutes*

## Installing tagtools for R

### Prerequisites

Before you begin, you must have R and—recommended—RStudio installed on
your computer. You can download this software at
[RStudio.com](https://posit.co/products/open-source/rstudio/#rstudio-desktop)
(you only need the free one).

Alternatively, you *can* work in [RStudio.cloud](https://rstudio.cloud).
This is free too, and it is nice that no download is required. The
drawback is that, with your free RStudio.cloud account, you are limited
at a certain number of project-hours per month, RAM, and CPU. One who
does a lot of work in RStudio, several hours a day, will run out of
hours within a week or two. The desktop version has fewer restraints.

If you happen to be new to R also,
[here](https://rstudio.cloud/learn/primers/1.2) is a very nice
standalone tutorial.

These practicals assume that you have some basic experience working with
R/RStudio, and can execute provided code, making some user-specific
changes along the way (e.g. to help R find a file you downloaded).

### Install tagtools from github

If you have `devtools` installed on your computer, you can use it to
install the `tagtools` package directly from github.

If you want or need to install `devtools`, or want more information
about it, see the [online installation guide at
r-project.org](https://www.r-project.org/nosvn/pandoc/devtools.html).

The code below gives the devtools command to install. Click “Code” at
the right to open the chunk, then you should be able to copy-paste the
code into your R console. **However**, be careful when copy-pasting
special characters such as \_underscores\_, and ‘quotes’. If you get an
error, one thing to check is that you have just a single, simple
underscore, and `'straight quotes'`, whether `'single'` or `"double"`
(rather than “smart quotes”).

``` r
devtools::install_github('animaltags/tagtools_r',
                         build_vignettes = FALSE)
```

If you are asked something like “These packages have more recent
versions available”, it is no harm to update them all, and to “install
from sources the packages which need compilation”.

With the package as of July 2021, the overall process should take ten to
fifteen minutes, depending on the speed of your internet and machine,
some eight minutes of which are spent building vignettes. Receiving an
abundance of red messages is not the end of the world—this is just how R
shows that a package is being installed. If it ends with the message
`"* DONE (tagtools)"`, you’re all set!

### Install tagtools from zip archive

If you do not want to use the `devtools` option, you can install the
`tagtools` package from the archive files provided with materials from
the WMMC 2019 workshop (in the folder created by unzipping
wmmc-2019-workshop-materials.zip). These archive files are also
available at the TagTools website. Try <http://animaltags.org> , or
<https://animaltags.netlify.app> .

*Note: if you choose to install from archive files (.zip or .tgz), you
will probably also have to manually install all the packages `tagtools`
depends on. If you use the `devtools` method above, you will not.*

- **Windows** users will want to use the **.zip** archive
- **Mac** or **Linux** users will want to use the **.tgz** archive

Save the appropriate archive to a known location on your computer.

Then, run:

``` r
install.packages('YourPath/YourFilename') # you'll have to edit this appropriately
```

(fill in the absolute or relative path to the archive file, and the
archive file name). For example, on a Mac, if the file is saved to
Downloads, your path might look like this:

``` r
install.packages('/Users/YourUsernameHere/Downloads/FileName.tgz')
```

You can also use the “install packages” GUI in RStudio—go to the
“Packages” tab, click “Install”, choose “Package Archive File” from the
“Install From” pulldown menu, and then navigate to the archive file.
Then follow the prompts.

You must also make sure you have installed all the packages that
`tagtools` depends on:

``` r
dpnds <- c('CircStats', 'ggformula', 'graphics', 'hht', 
           'latex2exp', 'lubridate', 'magrittr', 
           'matlab', 'ncdf4', 'plotly', 'pracma', 
           'readr', 'rgl', 'signal', 'stats', 
           'utils', 'zoo', 'zoom')
install.packages(pkgs = dpnds)
```

Note here that we are writing an object `dpnds`, which is a vector of
these 18 (text) strings, concatenated together with
[`c()`](https://rdrr.io/r/base/c.html). Then
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
can take all these dependencies as an input.

### Check tagtools Installation

Once the `tagtools` are successfully installed, you should be able to
run

``` r
library(tagtools)
```

You will often get this message: “The following object is masked from
‘package:stats’: smooth”—this means you have loaded it properly;
congratulations!

Another check is to make sure that function help files are accessible;
for example,

``` r
?load_nc
```

Now or at any point in the future when doing a check like this, you
might get an error saying the function doesn’t exist. When this happens,
make sure you have tagtools loaded in your library. While you won’t have
to reinstall the package with
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
unless you start working on a different machine, you will have to run
[`library(tagtools)`](https://animaltags.org) or
[`require(tagtools)`](https://animaltags.org) every time you restart
RStudio.

Having fewer packages loaded is kinder to your computer’s memory, so it
is good that R doesn’t automatically load everything. However, it does
mean that every time you restart RStudio, you must reload the packages
you need, such as `tagtools`, yourself. Thus at the beginning of each
vignette after this one you’ll be reminded to run
[`library(tagtools)`](https://animaltags.org).

If the command
[`? load_nc`](https://animaltags.github.io/tagtools_r/reference/load_nc.md)
pulls up a help file for the function `load_nc` like it should, you’re
all set.

## Accessing other vignettes through R directly

If you’ve run the `devtools` command, you can read these vignettes
directly in your local RStudio without needing to click through the
website! The syntax is very straightforward. To see the list of
vignettes in just this package, simply type:

``` r
vignette(package = "tagtools")
```

Show/Hide Results

    ## [1] "Vignettes in package ‘tagtools’:"
    ## [1] ""
    ## [1] "complementary-filtering         complementary-filtering (source, html)"
    ## [1] "data-quality-error-correction   data-quality-error-correction (source, html)"
    ## [1] "Detectors                       Detectors (source, html)"
    ## [1] "detectors-draft                 detectors-draft (source, html)"
    ## [1] "dive-stats                      dive-stats (source, html)"
    ## [1] "find-dives                      find-dives (source, html)"
    ## [1] "fine-scale-tracking             fine-scale-tracking (source, html)"
    ## [1] "install-load-tagtools           install-load-tagtools (source, html)"
    ## [1] "jerk-transients                 jerk-transients (source, html)"
    ## [1] "load-tag-data                   load-tag-data (source, html)"
    ## [1] "mahalanobis-distance            mahalanobis-distance (source, html)"
    ## [1] "more-filtering                  more-filtering (source, html)"
    ## [1] "plots-and-cropping              plots-and-cropping (source, html)"
    ## [1] "rotation-test                   rotation-test (source, html)"
    ## [1] "tag-to-whale-frame              tag-to-whale-frame (source, html)"
    ## [1] "vectors-vs-structures           vectors-vs-structures (source, html)"

Then you can run any vignette with
`vignette('package-index', package = 'tagtools')`. For instance, in
order to see this vignette, you can run:

``` r
vignette("install-load-tagtools", package = "tagtools")
```

The `package = "tagtools"` argument at the end is optional. However, it
is a good addition because it clarifies which package you are working
in. So, if there happened to be, say, another `vectors-vs-structures`
vignette in some other package, you could load the one that is specific
to this package with
`vignette("vectors-vs-structures", package = "tagtools")`.

## Review

What have you learned so far? Important setup steps that will help avoid
confusion and delay moving forward.

Congrats!

*If you’d like to continue working through these vignettes,
`load-tag-data` and `plots-and-cropping` are good options to help you
get started with the tools you’ve just installed and loaded.*

*Perhaps your best starting option is ‘load-tag-data’, helpful for
familiarizing yourself with the process of loading in data, as well as
making sure that important metadata is included in the data.*

``` r
vignette('load-tag-data', package = 'tagtools')
```

*`vectors-vs-structures` is a quick review of the way R stores data in
structures, and how these can be extracted to individual
vectors/scalars.*

``` r
vignette('vectors-vs-structures', package = 'tagtools')
```

*If you already feel confident with setup and structures in R, try
`plots-and-cropping`—you’ll get to start visualizing some data of the
types you’ve been learning to load.*

``` r
vignette('plots-and-cropping', package = 'tagtools')
```

------------------------------------------------------------------------

Animaltags tag tools online: <http://animaltags.org/>,
<https://github.com/animaltags/tagtools_r> (for latest beta source
code), <https://animaltags.github.io/tagtools_r/index.html> (vignettes
overview)
