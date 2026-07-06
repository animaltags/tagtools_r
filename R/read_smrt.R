#' Read SMRT tag data file(s) and convert to .nc
#'
#' Read data file(s) with data from a SMRT tag deployment, including associated metadata; apply initial calibration to convert to engineering units and store the resulting data in a .nc file.
#' @param data_dir string containing the name (including full or relative path) of the directory where the data files are stored. If omitted, \code{fname} must include full or relative path information for the csv file(s).
#' @param depid string containing the deployment identification code assigned
#' to this deployment, for example, 'mn12_186a'. If \code{fname} is not input, csv files are assumed to have names of the form "\code{depid}_001.csv" (002, 003 etc if multiple files) or "\code{depid}.csv" (if data is in a single file).
#' @param sm_dir directory where files from the SM board of the SMRT tag are stored (these are the files "unpacked" from the .dtg files, and include .swv, .xml, .csv, and .wav files). If not provided, the defaul will be subdirectory "SMfiles" within \code{data_dir}.
#' @param gps_file name of file (including path) with tag GPS data. If not provided, this function will search for a file in \code{data_dir} which includes the string "FastGPS.csv".
#' @param archive_file name of file (including path) with tag "Archive" data from the Wildlife Computers board. If not provided, this function will search for a file in \code{data_dir} which includes the string "Archive.csv".
#' @param tz string indicating the time zone used by the SMRT device clock. Default: "UTC".
#' @param save_SAtimes Logical. Save a .csv file containing metadata about the acoustic recordings (.wav files) from \code{sm_dir}? Default: TRUE.
#' @param SAtimes_fname Filename to which to save the metadata about acoustic .wav file data. Defaults to: "[depid]_wav_filetimes.csv" stored in the current working directory.
#' @param nc_dir String containing the name (including full or relative path) of the directory where the output nc file should be stored. Defaults to the current working directory.
#' @param nc_fname String containing the file name to use for the output netCDF file. Defaults to "(depid)_raw.nc" - for example, "mn12_186a_raw.nc"
#' @param info a list of deployment-specific metadata (create using \link[tagtools]{make_info}, potentially with manual additions after creation). Default: make_info(tagtype = "SMRT", depid = depid). This becomes the basis for the info structure of the output NetCDF file.
#' @param device_serial String containing the serial number of the SMRT tag. Obtained from \code{txt_fname} or else defaults to NULL; stored in the info structure of the output NetCDF file.
#' @param device_url String containing URL of tag manufacturer; defaults to "https://wildlifecomputers.com/" and is stored in the info structure of the output NetCDF file.

#' @return A string containing the file name of the netCDF (.nc) file in which the output has been saved. This function
#' generates a netCDF file in the current working directory containing
#' 		the tag data variables, including:
#' 		\itemize{
#' 		\item {A, Accelerometer data structure}
#' 		\item {M, Magnetometer data structure}
#' 		\item {depth, Depth data structure}
#' 		\item {temp, Temperature sensor data structure}
#' 		\item {SA, Metadata about acoustic recordings (.wav files)}
#' 		\item {info	Metadata about the deployment}
#' 		}
#' @note SMRT tags can produce very large csv files which are slow to
#' process. This function is (somewhat) optimised for speed and memory use so will
#' tolerate large files. But processing could be slow. Note also that data from 3D sensors 
#' **will be in the NEU orientation** expected by the animaltag tool kit. 
#' @export
#' @examples \dontrun{
#' nc_filename <- read_smrt()
#' load_nc(nc_filename)
#' }
read_smrt <- function(data_dir = NULL, 
                      depid,
                      sm_dir = paste(data_dir, "/SMfiles"),
                      gps_file = NULL,
                      archive_file = NULL,
                      tz = "UTC",
                      save_SAtimes = TRUE,
                      SAtimes_fname = paste(depid, "_wav_filetimes.csv", sep = ""),
                      nc_dir = getwd(),
                      nc_fname = paste(depid, "_raw.nc", sep = ""),
                      info = NULL,
                      device_serial = NULL,
                      device_url = "https://wildlifecomputers.com/") {
  
  # Input checking
  if (missing(depid)){
    stop("required input argument 'depid' is missing.")
  }
  
  # make sure there is not a / or \ or \\ at end of nc_dir
  nc_dir <- gsub(pattern = "[\\/*]$", replacement = "", x = nc_dir)
  # construct file name with path for output nc file
  nc_file <- file.path(nc_dir, nc_fname)
  
  if (file.exists(nc_file)){
    # if the file already exists, stop
    stop(paste("netCDF file", nc_file, "already exists. Delete it or choose a new netCDF file name."))
  }
  
  if (!is.null(data_dir)){
    # make sure there is not a / or \ or \\ at end of data_dir
    data_dir <- gsub(pattern = "[\\/*]$", replacement = "", x = data_dir)
  }
  
  # look for gps_file if not provided
  if (is.null(gps_file)){
    # if we have a data_dir look there for GPS data file
    if (!is.null(data_dir)){
      # try to find gps_file if filename not provided
      all_files <- list.files(data_dir, recursive = TRUE)
      gps_ix <- grepl(all_files, pattern = "FastGPS.csv", ignore.case = TRUE)
      if (sum(gps_ix) > 0){
        gps_file <- file.path(data_dir, all_files[gps_ix][1])
      }
      if (sum(gps_ix > 1)){
        warning(paste0(sum(gps_ix), " files with names including 'FastGPS.csv' found in data_dir; using: ", all_files[gps_ix][1],
                       ". Enter gps_file manually to read_smrt() to choose another."))
      }
    }
    if (is.null(gps_file)){
      warning("No files named '*FastGPS.csv' found in data_dir; please input gps_file if you want to include GPS data in netCDF file.")
    }
  }
  
  # look for archive_file if not provided
  if (is.null(archive_file)){
    # if we have a data_dir look there for WC "Archive" data file
    if (!is.null(data_dir)){
      # try to find archive_file if filename not provided
      all_files <- list.files(data_dir, recursive = TRUE)
      archive_ix <- grepl(all_files, pattern = "Archive.csv", ignore.case = TRUE)
      if (sum(archive_ix) > 0){
        archive_file <- file.path(data_dir, all_files[archive_ix][1])
      }
      if (sum(archive_ix > 1)){
        warning(paste0(sum(archive_ix), " files with names including 'Archive.csv' found in data_dir; using: ", all_files[archive_ix][1],
                       ". Enter archive_file manually to read_smrt() to choose another."))
      }
    }
    if (is.null(archive_file)){
      warning("No files named '*Archive.csv' found in data_dir; please input archive_file if you want to include data from WC board (depth, wet/dry sensor) in netCDF file.")
    }
  }
  
  if (is.null(info)){
    info <- tagtools::make_info(depid = depid, tagtype = "SMRT")
  }
  
  # try to get from xml file: device_serial
  if (!is.null(sm_dir) & dir.exists(sm_dir)){
    xml_info <- sm_get_config(sm_dir)
    # device serial number / ID number
    if (is.null(device_serial)){
      info$device_serial <- xml_info$device_serial
    }else{
      info$device_serial <- device_serial
    }
  }else{
    warning(paste("Directory ", sm_dir, "(for SM board data files) does not exist. Please check inputs to read_smrt()."))
  }
  
  # record start time as character string (from xml file)
  info$deploy_datetime_start <- xml_info$recording_start
  info$dephist_deploy_datetime_start <- info$deploy_datetime_start
  info$dephist_device_regset <- 'yyyy-mm-dd HH:MM:SS';
  ######################################## End of input checking
  
  # Read data from GPS file
  if (!is.null(gps_file)){
    if (file.exists(gps_file)){
      gps_sensor_list <- read_smrt_gps(gps_file)      
    }
  }

  # Read data from Archive file
  if (!is.null(archive_file)){
    if (file.exists(archive_file)){
      archive_sensor_list <- read_smrt_archive(depid = depid, info = info,
                                               archive_file = archive_file)      
    }
  }
  
  # Read data from SM files

  # TODO!!!!! Generate acoustic metadata file ("make_SA")
  
  ##### WORKING HERE on read_smrt_sm()
  # question: should read_smrt_x() RETURN a list of sensor lists or just save them into the NC file as they go?
  # saving as they go *might* be faster for large deployments?
  
  
  # check which sensors are present
  Sens <- c("Acc", "Mag", "Gyr", "Temp", "Depth", "Light")
  Sens_name <- c(
    "triaxial acceleration", "triaxial magnetometer", "triaxial gyroscope",
    "temperature", "pressure", "light level"
  )
  ax <- c(3, 3, 3, 1, 1, 1)

  # # add sensor data to nc file
  # for (k in c(1:nrow(sampling_rates))) {
  #     cols <- grep(sampling_rates$sensor_short_names[k], names(V))
  #     if (length(cols) > sampling_rates$naxes[k]){
  #       # if there are multiples of this sensor type
  #       # then the long name is taken from the CATS csv
  #       cols <- grep(pattern = sampling_rates$sensor_names[k], names(V), fixed = TRUE)
  #     }
  #     save_sens_struct(V[, cols], 
  #                      depid,
  #                      nc_file,
  #                      sampling_rate = sampling_rates[k, "fs"],
  #                      df = sampling_rates[k, "df"],
  #                      fname,
  #                      type = sampling_rates$sensor_short_names[k],
  #                      name = sampling_rates$unique_short_names[k],
  #                      description = sampling_rates$sensor_names[k],
  #                      naxes = sampling_rates$naxes[k]
  #     )
  # }
  # add_nc(nc_file, info, "info")
  # return(nc_file)
} # end of read_smrt

# HELPER FUNCTION to save SMRT sensor structure to nc file
# NOTE this is from read_cats...want to move it out if the same one can be used across tag types 
# (which would be ideal)
save_sens_struct <- function(X, depid, nc_file, sampling_rate, df = 1, fname, type, name, description = NULL, naxes) {
  if (is.null(names(X)) & naxes == 1){
    # if there is only one col it becomes a nameless vector
    # which causes trouble if we want to use the colnames for ordering the Acc Mag etc
    X = data.frame(X)
    names(X) <- type
  }
  
  if (ncol(X) != naxes){
    stop(paste("Mismatch between number of columns in data and expected number for sensor: ", type))
  }
  
  cols <- grep(type, names(X))
  if (length(cols) > 0) {
    if (length(cols) < naxes) {
      warning(sprintf(" Warning: %d axes of %s missing in data\n", naxes - length(cols), name))
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

