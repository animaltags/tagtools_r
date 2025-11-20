#' Read a CSV file with sensor data from a CATS tag
#'
#' Read in data from a CATS tag deployment (stored in one or more .csv files). This function is usable by itself but is more normally
#' called by \code{\link[tagtools]{read_cats}} which handles metadata and creates a NetCDF file.
#' @param fname is the file name of the CATS CSV file(s) including a full or relative path. The .csv suffix is optional.
#' @param max_samps is optional and is used to limit reading to a maximum number of samples (rows) per sensor. This is useful to read in a part of a very large file
#' for testing. If max_samps is not given, the entire file is read.
#' @param skip_samps Number of lines of data to skip (excluding header) before starting to read in data. Defaults to 0 (start at the beginning of the file), but could be used to read in a part of a file, or to read in and process a large file in chunks.
#' @return A tibble data frame containing the data read from the file. The column names are
#' taken from the first line of the CSV file and include units and axis. Some columns may be empty (if for example, a tag did not record data from a certain sensor type).
#' @note CATS csv files can be extremely large; perhaps too large to read the entire file into memory at once and work with it.
#' @export
  
read_cats_csv <- function(fname, max_samps = Inf, skip_samps = 0) {
  if (length(fname) > 1){
    # make sure file names are given in order
    fname <- sort(fname)
    # make sure there are no gaps in file numbers
    fnums <- as.numeric(
      sub(
        x = regmatches(
          x = basename(fname),
          regexpr(pattern = '\\d+\\.csv*$', 
                  text = basename(fname))
        ),
        pattern = '.csv$',
        replacement = ''
      )
    )
    
    if (any(diff(fnums) > 1)){
      warning('There are gaps in numbered .csv files. Concatenating files: \n', fname)
    }
  }
  
  # append .csv if needed
  need_suffix <- !grepl(fname, pattern = "\\.csv$")
  if (any(need_suffix)) {
    fname[need_suffix] <- paste(fname[need_suffix], ".csv", sep = "")
  }

  # read data file(s) and concatenate if multiple files
  V <- do.call(what = rbind, 
               lapply(X = fname, 
                      FUN = read1_cats_csv, 
                      skip_samps = skip_samps, 
                      max_samps = max_samps))
  return(V)
} # end of read_cats_csv()

# helper function to read a single CATS csv file
read1_cats_csv <- function(file, skip_samps, max_samps){
  V <- suppressMessages(
    readr::read_csv(
      file = file, 
      col_names = TRUE,
      col_types = readr::cols(
        `Time (UTC)` = readr::col_character(),
        # `GPS (raw) 1 [raw]` = readr::col_character(),
        # `GPS (raw) 2 [raw]` = readr::col_character(),
        # `GPS 1` = readr::col_character(),
        # `GPS 2` = readr::col_character()
      ),
      na = c(NA, "", " "),
      trim_ws = TRUE,
      skip = skip_samps,
      n_max = max_samps
    )) |>
    as.data.frame()
  # make mus and degree symbols and superscripts not show up as black diamonds with question marks inside
  # that make R throw errors...
  names(V) <- iconv(names(V), from = "UTF-8", to = "ASCII", sub = "")
  
  # add date-time in POSIX format
  di <- which(stringr::str_detect(names(V), "Date "))
  if (length(di) > 1){
    di <- which(stringr::str_detect(names(V), "Date ") & 
                  stringr::str_detect(names(V), "UTC"))
  }
  
  ti <- which(stringr::str_detect(names(V), "Time "))
  if (length(ti) > 1){
    ti <- which(stringr::str_detect(names(V), "Time ") & 
                  stringr::str_detect(names(V), "UTC"))
  }
  
  old_options <- options()
  on.exit(options(old_options))
  
  options(digits.secs = 6)
  V$Datetime <- as.POSIXct(paste(
    V[, di],
    V[, ti]),
    format = "%d.%m.%Y %H:%M:%OS", tz = 'UTC'
  )
  return(V)
} # end of read1_cats_csv()

