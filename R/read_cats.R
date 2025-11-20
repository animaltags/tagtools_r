
#' Read CATS csv data file(s) and convert to .nc
#'
#' Read .csv file(s) with data from a CATS tag deployment, including associated metadata, and store the resulting data in a .nc file.
#' @param file_dir String containing the name (including full or relative path) of the directory where the CATS csv file(s) are stored. If omitted, \code{fname} must include full or relative path information for the csv file(s).
#' @param depid String containing the deployment identification code assigned
#' to this deployment, for example, 'mn12_186a'. If \code{fname} is not input, csv files are assumed to have names of the form "\code{depid}_001.csv" (002, 003 etc if multiple files) or "\code{depid}.csv" (if data is in a single file).
#' @param fname Name(s) of the CATS csv file(s) to read. If \code{fname} is not provided, then the function will try to read all csv files in \code{file_dir}. If \code{file_dir} is provided, the path(s) to the file(s) will be constructed by appending the file name(s) to the \code{file_dir}. 
#' If \code{file_dir} is omitted, then \code{fname} is assumed to include the path to the file(s). The .csv file extension is optional.
#' @param txt_fname Name of the .txt file with metadata about the CATS deployment. If not input, the function will try to construct it from file_dir and depid (like \code{file.path(file_dir, paste0(depid, ".txt"))}). 
#' If present, this file will be used to determine sensor sampling rates; if not, sampling rates will be guessed based on timestamps in the csv file.
#' @param nc_dir String containing the name (including full or relative path) of the directory where the output nc file should be stored. Defaults to the current working directory.
#' @param device_serial String containing the serial number of the CATS tag. Obtained from \code{txt_fname} or else defaults to NULL; stored in the info structure of the output NetCDF file.
#' @param device_model_name String containing the model of the CATS tag used for data collection, for example "CATS Cam." Obtained from \code{txt_fname} or else defaults to NULL. This information is stored in the info structure of the output NetCDF file.
#' @param device_model_version String; CATS tag version. Obtained from \code{txt_fname} or else defaults to NULL; stored in the info structure of the output NetCDF file.
#' @param device_url String containing URL of tag manufacturer; defaults to "https://cats.is/" and is stored in the info structure of the output NetCDF file.
#' @param dephist_device_tzone String indicating the time zone in which the tag was deployed. Obtained from \code{txt_fname} or else defaults to NULL. Stored in the info structure of the output NetCDF file. For CATS tags this is the local offset from UTC time in hours.
#' @param animal_species_common Common name of species on which tag was deployed. Defaults to "unknonwn" and is stored in the info structure of the output NetCDF file.
#' @param animal_species_science Scientific name of species on which tag was deployed. Defaults to "unknonwn" and is stored in the info structure of the output NetCDF file.


#' @return A string (constructed by: '\code{depid}_raw.nc'; for example, 'mn12_186a_raw.nc') containing the file name of the netCDF (.nc) file in which the output has been saved. This function
#' generates a netCDF file in the current working directory containing
#' 		the tag data variables, including:
#' 		\itemize{
#' 		\item {A, Accelerometer data structure}
#' 		\item {M, Magnetometer data structure}
#' 		\item {temp, Temperature sensor data structure}
#' 		\item {info	Information structure for the deployment}
#' 		}
#' @note CATS loggers can produce very large csv files which are slow to
#' process. This function is (somewhat) optimised for speed and memory use so will
#' tolerate large files. But processing could be slow. Note also that although CATs tags use a NED axis orientation for 3D sensors, 
#' **this function converts to the NEU orientation** expected by the animaltag tool kit. 
#' To revert (if continuing analysis with CATs-specific tools outside animaltags), 
#' simply multiply all z-axis values by -1, and consider editing the metadata. 
#' Also note that according to Cade et al. 2021, not all CATs tags have the same internal orientation of the triaxial sensors -- 
#' such that the first column in the data may or may not be the "x axis." 
#' Here, we assume that the three columns of data for any triaxial sensor are correctly labeled with X,Y,Z included in the column name in the CATs csv file. 
#' If not, further data-based bench calibration of the device may be needed to determine correct axis orientation.
#' @export
#' @examples \dontrun{
#' nc_filename <- read_cats("my_cats_file.csv", "my_cats_deployment_name")
#' load_nc("my_cats_deployment_name_raw.nc")
#' }
read_cats <- function(file_dir = NULL, 
                      fname = NULL, 
                      depid, 
                      txt_fname = NULL,
                      nc_dir = getwd(),
                      device_serial = NULL,
                      device_model_name = NULL,
                      device_model_version = NULL,
                      device_url = "https://cats.is/",
                      dephist_device_tzone = NULL,
                      animal_species_common = "unknown",
                      animal_species_science = "unknown") {
  # Input checking
  if (missing(depid)){
    stop("required input argument 'depid' is missing.")
  }
  
  if (!is.null(file_dir)){
    # make sure there is not a / or \ or \\ at end of file_dir
    file_dir <- gsub(pattern = "[\\/*]$", replacement = "", x = file_dir)
  }
  
  if (is.null(txt_fname)){
    # if we have a file_dir look there for txt file
    if (!is.null(file_dir)){
      # try to guess txt_fname if not provided
      txt_fname <- file.path(file_dir, paste0(depid, ".txt"))
    }else{ # but if there is no file_dir either
      # check the current working dir
      txt_fname <- file.path(getwd(), paste0(depid, ".txt"))
    }
  }
  
  if (!file.exists(txt_fname)){
    warning(paste0("Could not find metadata file:\n", 
                   txt_fname,
                   ". Sensor sampling rates will be estimated based on csv data.\n"))
  }
  
  if (is.null(fname) & !is.null(file_dir)){
    # get file name(s) (with path included) 
    # that are in file_dir
    # and that contain depid and maybe a number at the end
    # files will be sorted alphabetically so should be in order from 1....n (if multiple)
    fname <- normalizePath(list.files(path = file_dir, 
                                      # csv file names include depid and then
                                      # maybe an _ or other separator
                                      # and then maybe a number eg 001, 002
                                      pattern = paste0( depid, '.*\\d*\\.csv$'), 
                                      full.names = TRUE))
    if (length(fname) == 0){
      # if there are no csv files in file_dir
      stop(paste0("No files found in folder: ", file_dir, "."))
    }
    # all info from file_dir is now in fname
    file_dir <- NULL
  }
  
  if (!is.null(file_dir)){
    # get file name(s) with path if user input both file_dir and fname
    fname <- file.path(file_dir, fname)
  }
  # End of input checking
  
  # Read metadata from txt file, if present
  # Note: the code for sensor_meta works, but data from it doesn't seem to match data file so ???
  if (file.exists(txt_fname)){
    cats_meta <- read.delim(txt_fname, header = FALSE)[,1]
    start_sensors <- grep(cats_meta, pattern = "[activated sensors]", fixed = TRUE)
    device_meta <- cats_meta[c(grep(cats_meta, pattern = "[device]", fixed = TRUE) :
                                 grep(cats_meta, pattern = "utc_offset", fixed = TRUE))]
    sensor_meta <-
      cats_meta[c(grep(cats_meta, pattern = "[activated sensors]", fixed = TRUE) :
                     length(cats_meta))]
    if (is.null(device_serial)){
      # the line starting with "sn=" has the serial number
      device_serial =
        device_meta[substr(device_meta, start = 1, stop = 3) == "sn="] |>
          substr(start = 4, stop = 100)
    }
    if (is.null(device_model_version)){
      # the line starting with "version=" has the version
      device_model_version <-
        device_meta[substr(device_meta, start = 1, stop = 8) == "version="] |>
        substr(start = 9, stop = 100)
    }
    if (is.null(dephist_device_tzone)){
      # the line starting with "utc_offset=" has UTC offset
      dephist_device_tzone =
        device_meta[substr(device_meta, start = 1, stop = 11) == "utc_offset="] |>
        substr(start = 12, stop = 100)
    }

    if (length(sensor_meta) > 1){
    #   get_fs <- function(data, sensor_meta){
    #     fs <- sub(pattern = data,
    #               x = sensor_meta[grep(sensor_meta, pattern = data, fixed = TRUE)],
    #               replacement = "") |>
    #       as.numeric()
    #     
    #   }
    #   sampling_rates <- data.frame(
    #     sensor_names = sensor_meta[grep(sensor_meta, pattern = "_name=", fixed = TRUE)])
    #   sampling_rates$sensor_nums <- substr(sampling_rates$sensor_names, start = 1, stop = 2)
    #   sampling_rates$sensor_names <- substr(sampling_rates$sensor_names, start = 9, stop = 100)
    #   sampling_rates$fs_name <- paste0(sampling_rates$sensor_nums, "_interval=")
    #   sampling_rates$fs <- sapply(sampling_rates$fs_name,
    #                               FUN = get_fs,
    #                               sensor_meta = sensor_meta)
    #   sampling_rates$df <- max(sampling_rates$df) / sampling_rates$df 
    sampling_rates <- NULL
    }else{ sampling_rates <- NULL }
  }else{ sampling_rates <- NULL }
 
  
  # Read data from csv file(s)
  V <- read_cats_csv(fname)
  
  # Set up metadata "info" structure
  info <- list(
    depid = depid,
    data_source = paste0(fname, collapse = ", "),
    data_nfiles = paste0(length(fname)),
    data_format = "csv",
    device_serial = device_serial,
    device_make = "CATS",
    device_type = "Archival",
    device_model_name = device_model_name,
    device_model_version = device_model_version,
    device_url = device_url,
    # note: could be determined by diff betw UTC and Local time in data
    # currently from user input or from txt file
    dephist_device_tzone = dephist_device_tzone,
    dephist_device_regset = "yyyy-mm-dddd HH:MM:SS",
    dephist_device_datetime_start = as.character(V$Datetime[1])
  )

  # check which sensors are present
  Sens <- c("Acc", "Mag", "Gyr", "Temp", "Depth", "Light")
  Sens_name <- c(
    "triaxial acceleration", "triaxial magnetometer", "triaxial gyroscope",
    "temperature", "pressure", "light level"
  )
  ax <- c(3, 3, 3, 1, 1, 1)
  # note: GPS are not well dealt with yet!! don't understand what info is in each col, 
  # need to consult docs but seems like this is the date, time and then 2 integers so maybe metadata?
  # according to Cade/Gough tools it's date, time, sat1, sat2
  sl <- sl_short <- list()
  SS <- vector("logical", length = length(Sens))

  for (k in c(1:length(Sens))) {
    cols <- grep(Sens[k], names(V))
    if (any(!is.na(V[, cols]))) {
      reps = 1 + length(cols) - ax[k]
      if (reps > 1){
        sl_entry <- names(V)[cols]
        sl_short_entry <- rep(Sens[k], reps)
      }else{
        sl_entry <- Sens_name[k]
        sl_short_entry <- Sens[k]
      }
      sl <- c(sl, sl_entry)
      sl_short <- c(sl_short, sl_short_entry)
      SS[k] <- TRUE
    }
    sl <- paste0(sl, collapse = ",")
    sl_short <- paste0(sl_short, collapse = ",")
  }
  
  if (is.null(sampling_rates)){
    # if only using csv file
    info$sensors_list <- sl    
  }else{
    # if we have metadata from txt file
    # this might include sensor names read_cats can't process (yet)
    info$sensors_list <- paste0(sampling_rates$sensor_names, collapse = ",")
  }

  nc_file <- file.path(nc_dir, 
                       paste(depid, "_raw.nc", sep = ""))

  
  # time stuff if we have to guess (if not gotten from txt file)
  if (is.null(sampling_rates)){
    dT <- as.numeric(difftime(utils::tail(V$Datetime, -1), 
                              utils::head(V$Datetime, -1),
                              units = 'secs')) 
    md <- stats::median(dT)
    km <- abs(dT - md) < 0.5 * md
    if (sum(km) < 0.75 * length(dT)) {
      warning("Many gaps in sampling. Inferred sampling rate may be inaccurate.\n")
    }
    # inferred sampling rate in Hertz
    # note this is of the MAX SENSOR and actually needs to be checked for each sensor
    # rows are repeated for many sensors so we need to remove the duplicates
    sampling_rates <- data.frame(sensor_names = unlist(strsplit(sl, ",")),
                                 sensor_short_names = unlist(strsplit(sl_short, ",")),
                                 fs = NA,
                                 df = NA,
                                 naxes = NA)
    sampling_rates$unique_short_names <- sampling_rates$sensor_short_names
    csv_fs <- round(1 / mean(dT[km]) * 1000) / 1000
    unum <- 1
    for (k in c(1:nrow(sampling_rates))){
      if (k > 1){
        if (sampling_rates$sensor_short_names[k] == 
            sampling_rates$sensor_short_names[k-1]){
          unum <- unum + 1
          sampling_rates$unique_short_names[k] <- paste0(sampling_rates$unique_short_names[k], unum)
        }else{
          unum <- 1
        }
      }
      cols <- grep(sampling_rates$sensor_short_names[k], names(V))
      myrle <- rle(V[, cols[1]])
      sampling_rates[k, "df"] <- unique(myrle$lengths)[which.max(table(myrle$lengths))]
      sampling_rates[k, "fs"] <- csv_fs / sampling_rates[k, "df"]
      sampling_rates[k, "naxes"] <- ax[Sens  == sampling_rates$sensor_short_names[k]]
    }
  }
  
  # add sensor data to nc file
  for (k in c(1:nrow(sampling_rates))) {
      cols <- grep(sampling_rates$sensor_short_names[k], names(V))
      if (length(cols) > sampling_rates$naxes[k]){
        # if there are multiples of this sensor type
        # then the long name is taken from the CATS csv
        cols <- grep(pattern = sampling_rates$sensor_names[k], names(V), fixed = TRUE)
      }
      save_sens_struct(V[, cols], 
                       depid, 
                       sampling_rate = sampling_rates[k, "fs"],
                       df = sampling_rates[k, "df"],
                       fname,
                       type = sampling_rates$sensor_short_names[k],
                       name = sampling_rates$unique_short_names[k],
                       description = sampling_rates$sensor_names[k],
                       naxes = sampling_rates$naxes[k]
      )
  }
  add_nc(nc_file, info, "info")
  return(nc_file)
} # end of read_cats

# HELPER FUNCTION to save CATS sensor structure to nc file
save_sens_struct <- function(X, depid, sampling_rate, df = 1, fname, type, name, description = NULL, naxes) {
  nc_file <- paste(depid, "_raw.nc", sep = "")
  if (is.null(names(X)) & naxes == 1){
    # if there is only one col it becomes a nameless vector
    # which causes trouble if we want to use the colnames for ordering the Acc Mag etc
    X = data.frame(X)
    names(X) <- type
  }
  cols <- grep(type, names(X))
  if (length(cols) > 0) {
    if (length(cols) < naxes) {
      warning(sprintf(" Warning: %d axes of %s missing in data\n", naxes - length(k), name))
    }
    if (naxes > 1) {
      # make sure column indices are ordered x, then y, then z
      # this assumes x, y, z cols of same sensor are named such that
      # alphabetical sorting --> x, y, z order
      cols <- cols[order(names(X)[cols])]
    } 
    # else {
    #   # o boy this is just taking the first temp which is the mag one!
    #   cols <- cols[1]
    # }
    
    if (grepl("gyr", name, ignore.case = TRUE)) {
      scf <- 0.001 # gyroscope unit is mrad/s. Multiply by 0.001 to get rad/s
    } else {
      scf <- 1 # all other units are standard
    }
    
    # pull names off data and make it a matrix
    cnames <- names(X)[cols]
    X <- as.matrix(X[, cols])
    
    # keep only unique samples as determined by sampling rate and max sampling rate
    # this is currently based on unique rows in sensor csv and NOT on txt metadata
    if (df > 1){
      X <- X[seq(from = 1, by = df, to = nrow(X)),]
    }
    
    if (naxes == 3){
      # for triaxial sensors need to change from NED to NEU orientation
      # (DELETE THIS PART or make it an input option dependent on tag type if ever moving this fn outside of read_cats!!)
      X <- X %*% matrix(c(1,0,0, 0,1,0, 0,0,-1), ncol = 3, byrow = TRUE)
    }
    
    dimnames(X) <- NULL

      S <- sens_struct(
        data = X,
        sampling_rate = sampling_rate, 
        depid = depid, 
        type = type, # sensor type (for looking up metadata)
        name = name, # name of the sensor struct
        description = description # more detail 
      )
      S$history <- "read_cats"
      S$files <- paste0(fname, collapse = ",")
      if (grepl("light", name, ignore.case = TRUE)) {
        S$unit <- "1"
        S$unit_name <- "counts"
        S$unit_label <- "counts"
      }
      
      add_nc(nc_file, S, name)
  }
} # end of save_sens_struct

