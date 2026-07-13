#' Read SMRT tag data from Wildlife Computers board (Archive file) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data data from Wildlife Computers board (Archive file) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param info list with metadata about the deployment (used to obtain recording start time)
#' @param archive_file name of file (including path) with WC board data (required)

#' @return A list of sensor data lists with sensor data, including:
#' 		\itemize{
#' 		\item {battery}
#' 		\item {wet_dry, wet/dry sensor data (1 means dry)}
#' 		}
#' @export
#' @examples \dontrun{
#' archive_data <- read_smrt_archive(depid, archive_file)
#' }
read_smrt_archive <- function(depid,
                          info,
                          archive_file) {
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop(
      "Package \"vroom\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # Input checking
  if (missing(depid) | missing(archive_file) | missing(info)){
    stop("read_smrt_archive() requires inputs depid, info, and archive_file")
  }
  
  if (!file.exists(archive_file)){
    stop(paste("File ", archive_file, " not found. Please check gps_file input to read_smrt_archive()." ))
  }
  
  recording_start <- lubridate::ymd_hms(info$deploy_datetime_start,
                                        tz = "UTC")
  
  archive_raw <- suppressMessages(
    vroom::vroom(archive_file, show_col_types = FALSE) )
  
  # get datetimes as datetime object in R
  archive_raw$datetime <- lubridate::mdy_hms(archive_raw$Time, tz = "UTC")
  # WC board may start a couple seconds before SM board.
  # to keep all sensors synced easily with acoustics, keep only data after SM start
  archive_raw <- archive_raw[archive_raw$datetime >= recording_start,]
  
  # which sensor data to collect and output
  sensor_var_names <- c("Battery", "dry")
  sensor_corrected_names = c('battery_voltage', 'wet_dry')
  sensor_axes <- c(1,1)
  
  # sampling rate of archive datafile
  n_check <- min(nrow(archive_raw) - 100, 10000)
  # start at 100th sample in case of irreg @ start; 
  # don't check more than max 10k rows (faster in case dataset is huge)
  archive_fs <- 1 / stats::median(
    as.numeric(
      difftime(
        archive_raw$datetime[100 + (2:n_check)],
        archive_raw$datetime[100 + (1:(-1+n_check))],
        units = "secs")))

  # make list object to hold output sensor data lists
  archive_data <- list()

  # make sensor data lists for each sensor in sensor_var_names
  for (s in c(1:length(sensor_var_names))){
    # if this sensor is in the dataset...
    this_sensor_ix <- stringr::str_starts(pattern = tolower(sensor_var_names[s]),
                            tolower(names(archive_raw)))
    if (sum(this_sensor_ix) > sensor_axes[s]){
      warning(paste("Archive data file contains ", sum(this_sensor_ix), 
              " variable names containing '",
              sensor_var_names[s], "'; using: ",
              names(archive_raw)[this_sensor_ix][c(1:sensor_axes[s])],
              sep = ""))
    }
    if (any(this_sensor_ix)){
      archive_data[[sensor_corrected_names[s]]] <- 
        suppressWarnings(
          sens_struct(
            data = as.matrix(archive_raw[, names(archive_raw)[this_sensor_ix][c(1:sensor_axes[s])]]),
            sampling_rate = archive_fs,
            depid = depid,
            type = sensor_corrected_names[s],
            name = sensor_corrected_names[s]))
          archive_data[[sensor_corrected_names[s]]]$history <-
            c(archive_data[[sensor_corrected_names[s]]]$history,
              "read_smrt_archive")
          archive_data[[sensor_corrected_names[s]]]$files <-
            basename(archive_file)
    }
  }
  
  return(archive_data)
} # end of read_smrt_archive()

