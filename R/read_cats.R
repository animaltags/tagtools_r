
#' Read CATS csv data file(s) and convert to .nc
#'
#' Read .csv file(s) with data from a CATS tag deployment, including associated metadata, and store the resulting data in a .nc file.
#' @param file_dir String containing the name (including full or relative path) of the directory where the CATS csv file(s) are stored. If omitted, \code{fname} must include full or relative path information for the csv file(s).
#' @param depid String containing the deployment identification code assigned
#' to this deployment, for example, 'mn12_186a'. If \code{fname} is not input, csv files are assumed to have names of the form "\code{depid}_001.csv" (002, 003 etc if multiple files) or "\code{depid}.csv" (if data is in a single file).
#' @param fname Name(s) of the CATS csv file(s) to read. If \code{fname} is not provided, then the function will try to read all csv files in \code{file_dir}. If \code{file_dir} is provided, the path(s) to the file(s) will be constructed by appending the file name(s) to the \code{file_dir}. 
#' If \code{file_dir} is omitted, then \code{fname} is assumed to include the path to the file(s). The .csv file extension is optional.
#' @param nc_dir String containing the name (including full or relative path) of the directory where the output nc file should be stored. Defaults to the current working directory.

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
read_cats <- function(file_dir = NULL, fname = NULL, depid, nc_dir = getwd()) {
  if (missing(depid)){
    stop("required input argument 'depid' is missing.")
  }
  
  if (is.null(fname) & !is.null(file_dir)){
    # get file name(s) (with path included) 
    # that are in file_dir
    # and that contain depid and maybe a number at the end
    # files will be sorted alphabetically so should be in order from 1....n (if multiple)
    fname <- normalizePath(list.files(path = file_dir, 
                                      pattern = paste0( depid, '.*\\d*\\.csv$'), 
                                      full.names = TRUE))
    if (length(fname) == 0){
      # if there are not numbered files look for 
      stop(paste0("No files found in folder: ", file_dir, "."))
    }
    # all info from file_dir is now in fname
    file_dir <- NULL
  }
  
  if (!is.null(file_dir)){
    # get file name(s) with path if user input both file_dir and fname
    fname <- file.path(file_dir, fname)
  }
  
  
  V <- read_cats_csv(fname)
  
  info <- list(
    depid = depid,
    data_source = paste0(fname, collapse = ", "),
    data_nfiles = paste0(length(fname)),
    data_format = "csv",
    device_serial = NULL,
    device_make = "CATS",
    device_type = "Archival",
    device_model_name = NULL,
    device_model_version = NULL,
    device_url = NULL,
    dephist_device_tzone = "0",
    dephist_device_regset = "dd-mm-yyyy HH:MM:SS",
    dephist_device_datetime_start = as.character(V$Datetime)[1]
  )

  # time stuff
  dT <- as.numeric(difftime(utils::tail(V$Datetime, -1), 
                            utils::head(V$Datetime, -1),
                            units = 'secs')) 
  md <- stats::median(dT)
  km <- abs(dT - md) < 0.5 * md
  if (sum(km) < 0.75 * length(dT)) {
    warning("Many gaps in sampling. Inferred sampling rate may be inaccurate.\n")
  }
  # inferred sampling rate in Hertz
  sampling_rate <- round(1 / mean(dT[km]) * 1000) / 1000

  # check which sensors are present
  Sens <- c("Acc", "Mag", "Gyr", "Temp", "Depth", "Light")
  Sens_name <- c(
    "3 axis Accelerometer", "3 axis Magnetometer", "3 axis Gyroscope",
    "Temperature", "Pressure", "Light level"
  )
  ax <- c(3, 3, 3, 1, 1, 1)
  # note: GPS are not well dealt with yet!!
  sl <- list()
  SS <- vector("logical", length = length(Sens))

  for (k in c(1:length(Sens))) {
    cols <- grep(Sens[k], names(V))
    if (any(!is.na(V[, cols]))) {
      sl <- paste(sl, Sens_name[k], ",", sep = "")
      SS[k] <- TRUE
    }
  }

  info$sensors_list <- sl
  nc_file <- file.path(nc_dir, 
                       paste(depid, "_raw.nc", sep = ""))

  # add sensor data to nc file
  for (k in c(1:length(Sens))) {
    if (SS[k]) {
      cols <- grep(Sens[k], names(V))
      save_sens_struct(V[, cols], depid, sampling_rate,
        fname,
        name = Sens[k], naxes = ax[k]
      )
    }
  }
  add_nc(nc_file, info, "info")
  return(nc_file)
} # end of read_cats

# HELPER FUNCTION to save CATS sensor structure to nc file
save_sens_struct <- function(X, depid, sampling_rate, fname, name, naxes) {
  nc_file <- paste(depid, "_raw.nc", sep = "")
  cols <- grep(name, names(X))
  if (length(cols) > 0) {
    if (length(cols) < naxes) {
      warning(sprintf(" Warning: %d axes of %s missing in data\n", naxes - length(k), name))
    }
    if (naxes > 1) {
      # make sure column indices are ordered x, then y, then z
      # this assumes x, y, z cols of same sensor are named such that
      # alphabetical sorting --> x, y, z order
      cols <- cols[order(names(X)[cols])]
    } else {
      cols <- cols[1]
    }
    if (grepl("gyr", name)) {
      scf <- 0.001 # gyroscope unit is mrad/s. Multiply by 0.001 to get rad/s
    } else {
      scf <- 1 # all other units are standard
    }
    # pull names off data and make it a matrix
    X <- as.matrix(X[, cols])
    if (naxes == 3){
      # for triaxial sensors need to change from NED to NEU orientation
      # (DELETE THIS PART or make it an input option dependent on tag type if ever moving this fn outside of read_cats!!)
      X <- X %*% matrix(c(1,0,0, 0,1,0, 0,0,-1), ncol = 3, byrow = TRUE)
    }
    dimnames(X) <- NULL
    S <- sens_struct(
      data = X,
      sampling_rate, depid = depid, type = name
    )
    S$history <- "read_cats"
    S$files <- fname
    if (grepl("light", name)) {
      S$unit <- "1"
      S$unit_name <- "counts"
      S$unit_label <- "counts"
    }
    
    add_nc(nc_file, S, name)
  }
} # end of save_sens_struct

