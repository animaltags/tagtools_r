# Load tag data

Welcome to the load tag data vignette! On behalf of the team behind
tagtools, thanks for taking some time to get to know our package. We
hope it facilitates your work.

In this vignette, you will gain confidence with using the tagtools to
create and load netCDF files, including editing metadata (important data
about the data).

*Estimated time for this vignette: about 20 minutes.*

These vignettes assume that you have some basic experience working with
R/Rstudio, and can execute provided code, making some user-specific
changes along the way (e.g. to help R find a file you downloaded). We
will provide you with quite a few lines. To boost your own learning, you
would do well to try and write them before opening what we give, using
this just to check your work. Additionally, be careful when copy-pasting
special characters such as \_underscores\_, and ‘quotes’. If you get an
error, one thing to check is that you have just single, simple
underscores, and ‘straight quotes’, whether ‘single’ or “double” (rather
than “smart quotes”).

## Loading an .nc File

### Load the test data set

For this example, you will load in the data file from a CATs tag. The
data file is already stored in a .nc file called ‘cats_test_raw.nc’. If
you want to run this example, download the “cats_test_raw.nc” file from
<https://github.com/animaltags/tagtools_data> and change the file path
to match where you’ve saved the files. Click Code at the right to see
how.

``` r
library(tagtools) # always start with this
cats_file_path <- "nc_files/cats_test_raw.nc"
MN <- load_nc(cats_file_path)
```

This especially concise two-line method should always work with the
installation of `tagtools` that we support, for all eight built-in
datasets that come with the package. We will make frequent use of this
method in these vignettes. It is especially nice because it saves you
some hassle (of tracking down where the dataset is saved and setting
your working directory there, or of figuring out what your working
directory is and moving your dataset there).

**However**, it is worth your while to understand how to do these steps
manually, though this requires its own separate mental task, because you
will have to do this in the real world when you get your own data from
somewhere else. You should keep track of what your working directory is
for other reasons, too. So, familiarize yourself with the following
commands:

``` r
getwd() # prints your current working directory. Often this will not be a tidy folder, but contain other things. So, to avoid that... 
setwd("/Path/To/Tidy/Working/Directory/In/Quotes") # on Mac or Linux, paths look like this, with forward slashes between folders. For instance:
setwd("/Library/Frameworks/R.framework/Versions/4.1/Resources/library/tagtools/extdata/") # depending on your system, this may set your working directory to the folder containing the built-in datasets
setwd("C:\Users\Sam Fynewever\Program Files") # on Windows, paths look like this, with a letter and colon at the beginning, then backslashes between folders
```

Additionally, your working directory should be visible at the top of the
console:

![](working_dir.png)

Now, if you have navigated to your working directory in your file finder
and/or ensured that the dataset file, `cats_test_raw.nc`, is located
there (whether by changing your working directory or copy-pasting the
file into your working directory), you can run load_nc “by hand”—see
code:

``` r
MN <- load_nc("cats_test_raw") # no .nc required this way!
```

All right! Whichever way you have just run `MN <- load_nc(...)`, this
creates an animaltag list object `MN` in your workspace. You can view it
in the Environment tab if working in RStudio.

![](MN_in_environment.png)

Or in the command line/console, type:

``` r
names(MN)
str(MN, max.level = 1)
str(MN$A)
```

Show/Hide Results

    #> [1] "First the results for names(MN):"
    #> [1] "--------------------------------------------"
    #> [1] "A"    "M"    "G"    "LL"   "P"    "info"
    #> [1] "Then the results for str(MN, max.level = 1):"
    #> [1] "--------------------------------------------"
    #> List of 6
    #>  $ A   :List of 19
    #>  $ M   :List of 19
    #>  $ G   :List of 19
    #>  $ LL  :List of 19
    #>  $ P   :List of 19
    #>  $ info:List of 11
    #>  - attr(*, "class")= chr [1:2] "animaltag" "list"
    #> [1] "and finally the results for str(MN$A):"
    #> [1] "--------------------------------------------"
    #> List of 19
    #>  $ data              : num [1:720000, 1:3] -2.42 -2.4 -2.35 -2.3 -2.2 ...
    #>  $ sampling          : chr "regular"
    #>  $ sampling_rate     : num 400
    #>  $ sampling_rate_unit: chr "Hz"
    #>  $ depid             : chr "cats_test_raw"
    #>  $ creation_date     : chr "2023-06-14 12:58:08.013504"
    #>  $ history           : chr "read_cats"
    #>  $ type              : chr "A"
    #>  $ full_name         : chr "Acceleration"
    #>  $ description       : chr "triaxial acceleration"
    #>  $ unit              : chr "m/s2"
    #>  $ unit_name         : chr "metres per second squared"
    #>  $ unit_label        : chr "m/s^2"
    #>  $ start_offset      : num 0
    #>  $ start_offset_units: chr "second"
    #>  $ column_name       : chr "x,y,z"
    #>  $ frame             : chr "tag"
    #>  $ axes              : chr "FRU"
    #>  $ files             : chr "20160730-091117-Froback-11-part.csv"

You should see that variables Acc, Gyr, Depth, Light, and info are
contained within the list MN.

## Explore the test dataset

Now you can find out what each of the variables is by just typing their
name followed by enter. For example, for info (tag-wide metadata), run

``` r
MN$info
```

Show/Hide Results

    #> $creation_date
    #> [1] "2023-06-14 12:58:11.202458"
    #> 
    #> $depid
    #> [1] "cats_test_raw"
    #> 
    #> $data_source
    #> [1] "20160730-091117-Froback-11-part.csv"
    #> 
    #> $data_nfiles
    #> [1] "1"
    #> 
    #> $data_format
    #> [1] "csv"
    #> 
    #> $device_make
    #> [1] "CATS"
    #> 
    #> $device_type
    #> [1] "Archival"
    #> 
    #> $dephist_device_tzone
    #> [1] "0"
    #> 
    #> $dephist_device_regset
    #> [1] "dd-mm-yyyy HH:MM:SS"
    #> 
    #> $dephist_device_datetime_start
    #> [1] "2016-07-27 16:29:38"
    #> 
    #> $sensors_list
    #> [1] "3 axis Accelerometer,3 axis Magnetometer,3 axis Gyroscope,Temperature,Pressure,Light level,"

The `info` structure tells you where the data came from, when it was
collected, on what animal, and with what type of tag. info is the
general metadata store for a dataset. You can find out what is in a
specific field in info, e.g., to get the `sensors_list`:

``` r
MN$info$sensors_list
```

Show/Hide Results

    #> [1] "3 axis Accelerometer,3 axis Magnetometer,3 axis Gyroscope,Temperature,Pressure,Light level,"

### Updating and adding metadata

There is a lot of metadata missing from this file!
[`read_cats()`](https://animaltags.github.io/tagtools_r/reference/read_cats.md)
can only auto-fill it with information that is standard for all CATS
tags, or contained in the file itself. Other metadata must be added by
the user.

For example, to add a field called ‘data_owner’ with value ‘Jeremy
Goldbogen, Dave Cade’, you would enter:

``` r
MN$info$data_owner <- 'Jeremy Goldbogen, Dave Cade'
MN$info$data_owner
```

Show/Hide Results

    #> [1] "Jeremy Goldbogen, Dave Cade"

This is a way to add metadata to a dataset before sending it off to an
archive (e.g., a dataset to accompany a journal paper). Although there
are no rules for what you call each field, we are working toward
establishing a set of best practices.

Entering metadata by hand by typing it into the R console is tedious,
and also a recipe for inconsistencies. (Let’s avoid that!) To that end,
we have tried to provide a couple of other options, considered below.

### Saving Metadata with make_info

You can use
[`make_info()`](https://animaltags.github.io/tagtools_r/reference/make_info.md)
to create a metadata info structure with information about the
researcher, tag type, and study species. This function allows the user
to generate a “skeleton” info structure for a tag deployment, with some
common pieces of metadata filled in. Additional information can then be
added manually or using a custom script before saving this info as part
of a netCDF (.nc) file.

For example,

``` r
more_info <- make_info(depid = 'cats_test', tagtype = 'CATS', 
                       species = 'mn', owner = 'jg')
# results a few chunks later
```

Note that you may need to add researchers, species, and perhaps tag
types to the template files. To find out where these are stored on your
machine, you can run:

``` r
system.file('extdata', 'researchers.csv', package='tagtools')
```

Show/Hide Results

    #> [1] "/private/var/folders/jg/52183yqn5k32t7qcdwc0wkt40000gp/T/Rtmptdt9AO/temp_libpath51b0a765efd/tagtools/extdata/researchers.csv"

Then, navigate to the folder and make any edits that you would like. You
may need to restart R before they will take effect.

Once you have succeeded in running
[`make_info()`](https://animaltags.github.io/tagtools_r/reference/make_info.md),
you can view the results:

``` r
more_info
```

Show/Hide Results

    #> $animal_dbase_url
    #> [1] NA
    #> 
    #> $animal_id
    #> [1] "Unknown"
    #> 
    #> $animal_species_common
    #> [1] "Humpback whale"
    #> 
    #> $animal_species_science
    #> [1] "Megaptera novaeangliae"
    #> 
    #> $dephist_deploy_datetime_start
    #> [1] "UNKNOWN"
    #> 
    #> $dephist_deploy_locality
    #> [1] ""
    #> 
    #> $dephist_deploy_location_lat
    #> [1] ""
    #> 
    #> $dephist_deploy_location_lon
    #> [1] ""
    #> 
    #> $dephist_deploy_method
    #> [1] ""
    #> 
    #> $dephist_device_datetime_start
    #> [1] "UNKNOWN"
    #> 
    #> $dephist_device_regset
    #> [1] "dd/mm/yyyy HH:MM:SS"
    #> 
    #> $dephist_device_tzone
    #> [1] "0"
    #> 
    #> $depid
    #> [1] "cats_test"
    #> 
    #> $device_make
    #> [1] "CATS"
    #> 
    #> $device_model
    #> [1] ""
    #> 
    #> $device_serial
    #> [1] "UNKNOWN"
    #> 
    #> $device_type
    #> [1] "Archival"
    #> 
    #> $device_url
    #> [1] "http://www.cats.is/"
    #> 
    #> $dtype_datetime_made
    #> [1] "2025-12-01 20:14:00"
    #> 
    #> $dtype_format
    #> [1] ".csv"
    #> 
    #> $dtype_nfiles
    #> [1] "UNKNOWN"
    #> 
    #> $dtype_source
    #> [1] "UNKNOWN"
    #> 
    #> $project_datetime_end
    #> [1] ""
    #> 
    #> $project_datetime_start
    #> [1] ""
    #> 
    #> $project_name
    #> [1] ""
    #> 
    #> $provider_cite
    #> [1] "Contact data provider"
    #> 
    #> $provider_details
    #> [1] "Stanford University"
    #> 
    #> $provider_doi
    #> [1] "Contact data provider"
    #> 
    #> $provider_email
    #> [1] "jergold@stanford.edu"
    #> 
    #> $provider_license
    #> [1] "Contact data provider"
    #> 
    #> $provider_name
    #> [1] "Jeremy Goldbogen"
    #> 
    #> $sensors_firm
    #> [1] "Not specified"
    #> 
    #> $sensors_list
    #> [1] "3axis Accelerometer,3 axis Magnetometer,3 axis Gyroscope,Pressure,Light,GPS,Video"
    #> 
    #> $sensors_soft
    #> [1] "Not specified"

### Combining info and more_info

Now we have two different info structures—the one we just created with
[`make_info()`](https://animaltags.github.io/tagtools_r/reference/make_info.md)
and the one we got when we created the .nc file using
[`read_cats()`](https://animaltags.github.io/tagtools_r/reference/read_cats.md).

We can combine the two together, keeping the fields from the original
info if there are any duplicates:

``` r
more_info[names(MN$info)] <- MN$info
MN$info <- more_info
MN$info
```

Show/Hide Results

    #> $animal_dbase_url
    #> [1] NA
    #> 
    #> $animal_id
    #> [1] "Unknown"
    #> 
    #> $animal_species_common
    #> [1] "Humpback whale"
    #> 
    #> $animal_species_science
    #> [1] "Megaptera novaeangliae"
    #> 
    #> $dephist_deploy_datetime_start
    #> [1] "UNKNOWN"
    #> 
    #> $dephist_deploy_locality
    #> [1] ""
    #> 
    #> $dephist_deploy_location_lat
    #> [1] ""
    #> 
    #> $dephist_deploy_location_lon
    #> [1] ""
    #> 
    #> $dephist_deploy_method
    #> [1] ""
    #> 
    #> $dephist_device_datetime_start
    #> [1] "2016-07-27 16:29:38"
    #> 
    #> $dephist_device_regset
    #> [1] "dd-mm-yyyy HH:MM:SS"
    #> 
    #> $dephist_device_tzone
    #> [1] "0"
    #> 
    #> $depid
    #> [1] "cats_test_raw"
    #> 
    #> $device_make
    #> [1] "CATS"
    #> 
    #> $device_model
    #> [1] ""
    #> 
    #> $device_serial
    #> [1] "UNKNOWN"
    #> 
    #> $device_type
    #> [1] "Archival"
    #> 
    #> $device_url
    #> [1] "http://www.cats.is/"
    #> 
    #> $dtype_datetime_made
    #> [1] "2025-12-01 20:14:00"
    #> 
    #> $dtype_format
    #> [1] ".csv"
    #> 
    #> $dtype_nfiles
    #> [1] "UNKNOWN"
    #> 
    #> $dtype_source
    #> [1] "UNKNOWN"
    #> 
    #> $project_datetime_end
    #> [1] ""
    #> 
    #> $project_datetime_start
    #> [1] ""
    #> 
    #> $project_name
    #> [1] ""
    #> 
    #> $provider_cite
    #> [1] "Contact data provider"
    #> 
    #> $provider_details
    #> [1] "Stanford University"
    #> 
    #> $provider_doi
    #> [1] "Contact data provider"
    #> 
    #> $provider_email
    #> [1] "jergold@stanford.edu"
    #> 
    #> $provider_license
    #> [1] "Contact data provider"
    #> 
    #> $provider_name
    #> [1] "Jeremy Goldbogen"
    #> 
    #> $sensors_firm
    #> [1] "Not specified"
    #> 
    #> $sensors_list
    #> [1] "3 axis Accelerometer,3 axis Magnetometer,3 axis Gyroscope,Temperature,Pressure,Light level,"
    #> 
    #> $sensors_soft
    #> [1] "Not specified"
    #> 
    #> $creation_date
    #> [1] "2023-06-14 12:58:11.202458"
    #> 
    #> $data_source
    #> [1] "20160730-091117-Froback-11-part.csv"
    #> 
    #> $data_nfiles
    #> [1] "1"
    #> 
    #> $data_format
    #> [1] "csv"
    #> 
    #> $data_owner
    #> [1] "Jeremy Goldbogen, Dave Cade"

### Saving metadata with a GUI editor

Another option for entering user-generated metadata is to use the
animaltags metadata editor. This interface allows you to type your
information into pre-named fields, allowing you to fully customize the
information while also using standard field names.

To launch the editor, run:

``` r
metadata_editor()
```

A web browser window will open in which you can fill in metadata fields
for your tag.

![](meta_editor.png)

Depending on your individual platform, this editor may not have any of
the tabs open. Simply click on one of the tabs (Device Information, Tag
and Animal Information, etc.) to open it.

Perhaps you don’t currently have metadata on hand to fill in the fields
yourself. Or, at some point you might already have metadata stored in a
CSV spreadsheet, *in the specific format that this reader recognizes*
(it will not magically pick out metadata saved in a CSV in any order of
your choosing). In either case, the function that reads metadata for you
from a `.csv` file is useful. There should be a .csv spreadsheet with
metadata in your workshop materials that you can use for now. Upload it
with “Browse”, then click “Open from CSV” to harvest the data. This will
automate the data entry process for you.

When you are done, scroll to the bottom of the editor, fill in a file
name under which to save your work, and click “Save”.

*Note that there are a number of fields which we strongly suggest
filling out…you will get a warning if you try to save with these left
blank.* You can choose where on your computer to save this (temporary)
file, and it should confirm what the file name is.

Then, once this file is in your working directory, you can load the info
into R as an animaltags info structure via:

``` r
yet_more_info <- csv2struct('the_file_you_just_saved.csv')
# results not included, will depend on your individual file
```

And finally, combine it with the rest as before. Try to emulate the two
lines from above, changing the names as needed, without opening the
chunk below. Then check your work against this:

``` r
yet_more_info[names(MN$info)] <- MN$info
MN$info <- yet_more_info
```

## Saving the data

We’ve now made quite a few additions to the metadata in the `info`
structure, and we probably want to save those to our .nc file. Here’s
how.

``` r
MN$info[is.na(MN$info)] <- NULL
add_nc('cats_test_raw', X = MN$info, vname = 'info') 
```

That’s it! If all has worked smoothly, your .nc file will contain all
the additions we’ve been working on.

## Review

What have you learned so far? More important steps that will hopefully,
as you’re working with data more frequently, become healthy routines of
consistent, rigorous documentation.

Great work!

*If you’d like to continue working through these practicals,
`vectors-vs-structures` and `plots-and-cropping` are two good choices.*

*`vectors-vs-structures` is a quick review of the way R stores data in
structures, and how these can be extracted to individual
vectors/scalars.*

``` r
vignette('vectors-vs-structures', package = 'tagtools')
```

*In `plots-and-cropping`, you’ll get to start visualizing some data of
the types you’ve been learning to load.*

``` r
vignette('plots-and-cropping', package = 'tagtools')
```

------------------------------------------------------------------------

Animaltags tag tools online: <http://animaltags.org/>,
<https://github.com/animaltags/tagtools_r> (for latest beta source
code), <https://animaltags.github.io/tagtools_r/index.html> (vignettes
overview)
