#' Read SMRT tag data file(s) and convert to .nc
#'
#' Read data file(s) with data from a SMRT tag deployment, including associated metadata; apply initial calibration to convert to engineering units and store the resulting data in a .nc file.
#' @param data_dir string containing the name (including full or relative path) of the directory where the data files are stored. If omitted, \code{sm_dir}, \code{gps_file}, and \code{archive_file} must include full or relative path information for the csv file(s).
#' @param depid string containing the deployment identification code assigned
#' to this deployment, for example, 'mn12_186a'. If \code{fname} is not input, csv files are assumed to have names of the form "\code{depid}_001.csv" (002, 003 etc if multiple files) or "\code{depid}.csv" (if data is in a single file).
#' @param sm_dir directory where files from the SM board of the SMRT tag are stored (these are the files "unpacked" from the .dtg files, and include .swv, .xml, .csv, and .wav files). If not provided, the defaul will be subdirectory "SMfiles" within \code{data_dir}.
#' @param gps_file name of file (including path) with tag GPS data. If not provided, this function will search for a file in \code{data_dir} which includes the string "FastGPS.csv".
#' @param archive_file name of file (including path) with tag "Archive" data from the Wildlife Computers board. If not provided, this function will search for a file in \code{data_dir} which includes the string "Archive.csv".
#' @param read_gps logical: read GPS data? Default: TRUE. If FALSE, input \code{gps_file} will be ignored.
#' @param read_archive logical: read WC Archive file (wet-dry sensor and battery)? Default: TRUE. If FALSE, input \code{archive_file} will be ignored.
#' @param read_sm_swv logical: read data from swv files recorded by the SM board (e.g. accelerometer, magnetometer)? Default: TRUE.
#' @param read_sm_csv logical: read data from csv files recorded by the SM board (e.g. depth and temperature sensors). Default: TRUE. 
#' @param ch passed to \code{\link{read_smrt_sm}}. Default: NULL to read all channels recorded in swv files.
#' @param recn passed to \code{\link{read_smrt_sm}}. Default: NULL to read all swv files.
#' @param df passed to \code{\link{read_smrt_sm}}. Default: 1, to do no decimation of swv sensor data.
#' @param tz string indicating the time zone used by the SMRT device clock. Default: "UTC".
#' @param save_SAtimes Logical. Save a .csv file containing metadata about the acoustic recordings (.wav files) from \code{sm_dir}? Default: FALSE (this info is already stored in the SA structure within the nc file...).
#' @param SAtimes_file Filename to which to save the metadata about acoustic .wav file data. Defaults to: "[depid]_wav_filetimes.csv" stored in the current working directory.
#' @param nc_dir String containing the name (including full or relative path) of the directory where the output nc file should be stored. Defaults to the current working directory.
#' @param nc_file String containing the file name to use for the output netCDF file. Defaults to "(depid)_raw.nc" - for example, "mn12_186a_raw.nc"
#' @param info a list of deployment-specific metadata (create using \link[tagtools]{make_info}, potentially with manual additions after creation). Default: make_info(tagtype = "SMRT", depid = depid). This becomes the basis for the info structure of the output NetCDF file.
#' @param device_serial String containing the serial number of the SMRT tag. Obtained via a call to \code{\link{sm_get_config}} or else defaults to NULL; stored in the info structure of the output NetCDF file.
#' @param device_url String containing URL of tag manufacturer; defaults to "https://wildlifecomputers.com/" and is stored in the info structure of the output NetCDF file.

#' @note Currently this function reads in all sensors from all data file types (SM board sensors from swv files, WC board data recorded by the SM board from csv files, and GPS data from the WC Archive file.) In the future input options might be added to allow the user to specify which file/sensor types are to be read, and which temporal subset of data to return.
#' @return A string containing the file name of the netCDF (.nc) file in which the output has been saved. This function
#' generates a netCDF file in the current working directory containing
#' 		the tag data variables, which generally includes:
#' 		\itemize{
#' 		\item {A, Accelerometer data structure}
#' 		\item {M, Magnetometer data structure}
#' 		\item {depth, Depth data structure}
#' 		\item {temp, Temperature sensor data structure}
#' 		\item {wet_dry, Wet-dry sensor data structure}
#' 		\item {batt, Battery voltage data structure}
#' 		\item {SA, Metadata about acoustic recordings (.wav files)}
#' 		\item {info	Metadata about the deployment}
#' 		}
#' @note SMRT tags can produce very large data files which are slow to
#' process. This function is (somewhat) optimized for speed and memory use so will
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
                      read_gps = TRUE,
                      read_archive = TRUE,
                      read_sm_swv = TRUE,
                      read_sm_csv = TRUE,
                      ch = NULL,
                      recn = NULL,
                      df = 1,
                      tz = "UTC",
                      save_SAtimes = FALSE,
                      SAtimes_file = paste(depid, "_wav_filetimes.csv", sep = ""),
                      nc_dir = getwd(),
                      nc_file = paste(depid, "_raw.nc", sep = ""),
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
  nc_file <- file.path(nc_dir, nc_file)
  
  if (file.exists(nc_file)){
    # if the file already exists, stop
    stop(paste("netCDF file", nc_file, "already exists. Delete it or choose a new netCDF file name."))
  }
  
  if (!is.null(data_dir)){
    # make sure there is not a / or \ or \\ at end of data_dir
    data_dir <- gsub(pattern = "[\\/*]$", replacement = "", x = data_dir)
  }
  
  if (read_gps){
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
  }
  
  if (read_archive){
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
  }
  
  if (is.null(info)){
    info <- tagtools::make_info(depid = depid, tagtype = "SMRT")
  }
  
  # try to get from xml file: device_serial
  xml_info <- sm_get_config(sm_dir)
  if (!is.null(sm_dir) & dir.exists(sm_dir)){
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
  ######################################## End of input checking/processing
  
  # Read data from GPS file
  if (read_gps & !is.null(gps_file)){
    if (file.exists(gps_file)){
      gps_sensor_list <- read_smrt_gps(depid, info, gps_file)      
    }else{
      stop(paste0("GPS data file ", gps_file, " not found"))
    }
  }
  
  # Read data from Archive file
  if (read_archive & !is.null(archive_file)){
    if (file.exists(archive_file)){
      archive_sensor_list <- read_smrt_archive(depid = depid, info = info,
                                               archive_file = archive_file)      
    }else{
      stop(paste0("Archive file ", archive_file, " not found"))
    }
  }
  
  # Read data from SM files
  # Generate acoustic metadata file ("make_SA")
  SA <- sound_archive(sm_dir, depid)
  
  if (save_SAtimes){
    pam_times <- data.frame(SA$data)
    names(pam_times) <- c('FileNum', 'SecSinceStart', 'Samples', 'RowStatus')
    pam_times$DurSeconds <- pam_times$Samples / SA$sampling_rate
    pam_times$StartTime <- xml_info$recording_start + lubridate::seconds(pam_times$DurSeconds)
    pam_times$RowStatus <- as.character(pam_times$RowStatus)
    pam_times$RowStatus <- sapply(pam_times$RowStatus,
                                  function(x) switch(x,
                                                     `1` = "Data",
                                                     `0` = "O-filled",
                                                     `-1` = "Gap"))
    utils::write.csv(pam_times, file = SAtimes_file)
  }
  
  # read in data from data files in sm_dir (from SM board/swv files + csv files of WC board data recorded by SM board)
  # sm_data will be a list with entries:
  #  csv_data (data.frame; from csv files)
  #  swv_data (list; from swv files)
  # add options to read_smrt_sm for read_swv and read_
  sm_data <- read_smrt_sm(depid = depid, sm_dir = sm_dir, 
                          read_sm_csv = read_sm_csv, 
                          read_sm_swv = read_sm_swv, 
                          ch = ch, recn = recn, df = df)
  
  if (read_sm_swv | read_sm_csv){
    if (exists("sm_data", mode = "list")){
    save_sensor_lists(nc_file = nc_file, sensor_list_list = sm_data)
    }
  }
  
  if (read_gps & exists("gps_sensor_list", mode = "list")){
    save_sensor_lists(nc_file = nc_file, sensor_list_list = gps_sensor_list)
  }
  
  if (read_archive & exists("archive_sensor_list", mode = "list")){
    save_sensor_lists(nc_file = nc_file, sensor_list_list = archive_sensor_list)
  }

  # helper function to save a list of sensor data lists whether or not it's the first one
  save_sensor_lists <- function(nc_file, sensor_list_list){
    if ("info" %in% names(sensor_list_list)){
      info_struct <- sensor_list_list[["info"]]
      sensor_list_list[["info"]] <- NULL
    }
    if (!file.exists(nc_file)){
      save_nc(nc_file, sensor_list_list)
    }else{
      for (s in c(1:length(sensor_list_list))){
        add_nc(nc_file, sensor_list_list[[s]]) 
      }
    }
    # save info last if there was an info struct
    if (exists("info_struct")){
      add_nc(nc_file, info_struct)
    }
  }
  
  return(nc_file)
} # end of read_smrt
