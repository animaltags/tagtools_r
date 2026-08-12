#' Read SMRT tag GPS data file and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data file with  GPS data from a SMRT tag deployment, including associated metadata.
#' @param depid Deployment ID string
#' @param info list with metadata about the deployment (used to obtain recording start time)
#' @param gps_file name of file (including path) with tag GPS data (required)

#' @return A list of sensor data lists with GPS data, including:
#' 		\itemize{
#' 		\item {GPS_position, latitude and longitude}
#' 		\item {GPS_satellites, number of satellites per position}
#' 		\item {GPS_residual, GPS position residuals}
#' 		\item {GPS_time_error, GPS time error}
#' 		}
#' @export
#' @examples \dontrun{
#' gps_data <- read_smrt_gps(gps_file)
#' }
read_smrt_gps <- function(depid,
                          info,
                          gps_file) {
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop(
      "Package \"vroom\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # Input checking
  if (missing(depid) | missing(info) | missing(gps_file)){
    stop("read_smrt_gps() requires inputs depid, info, and gps_file")
  }
  
  if (!file.exists(gps_file)){
    stop(paste("File ", gps_file, " not found. Please check gps_file input to read_smrt_gps()." ))
  }
  
  recording_start <- lubridate::ymd_hms(info$deploy_datetime_start,
                                        tz = "UTC")
  
  gps_raw <- suppressMessages(
    vroom::vroom(gps_file, col_names = TRUE, skip = 3, show_col_types = FALSE,
                 col_types = vroom::cols(Day = 'character', Time = 'character')) )
  
  # rename some columns to standardize names...
  names(gps_raw) <- stringr::str_replace_all(names(gps_raw), pattern = " ", replacement = "")
  
  names(gps_raw)[grepl(names(gps_raw), pattern = "Bad_Sats")] <- "BadSats"
  names(gps_raw)[grepl(names(gps_raw), pattern = "Time_Error")] <- "TimeError"
  
  # which variables should be numeric
  num_cols <- names(gps_raw) %in% c('Name',
                                    'BadSats',
                                    'Latitude',
                                    'Longitude',
                                    'Satellites',
                                    'Residual',
                                    'TimeError')
  # make sure they ARE numeric
  gps_raw[,num_cols] <- suppressWarnings(apply(X = gps_raw[,num_cols], MARGIN = 2, FUN = as.numeric, simplify = FALSE))
  
  # combine Day and Time to get a datetime timestamp
  gps_raw$datetime <- lubridate::dmy_hms(paste(gps_raw$Day, gps_raw$Time), tz = "UTC")
  
  # compute times as seconds since start time
  gps_raw$time_sec <- as.numeric(
    difftime(gps_raw$datetime, recording_start, units = "secs"))
  
  gps_raw$BadSats[is.na(gps_raw$BadSats)] <- 0
  gps_raw$total_satellites <- gps_raw$Satellites - gps_raw$BadSats
  
  # make list object to hold output sensor data lists
  gps_data <- list()
  
  # add GPS positions
  if ("Latitude" %in% names(gps_raw) &
      "Longitude" %in% names(gps_raw) &
      "time_sec" %in% names(gps_raw)) {
    gps_data$GPS_position <- 
      sens_struct(as.matrix(gps_raw[,c("Latitude", "Longitude")]),
                  times = gps_raw$time_sec,
                  depid = depid,
                  type = "Position",
                  name = "GPS_position",
                  description = "GPS_position")
    gps_data$GPS_position$history <- paste(gps_data$GPS_position$history, "read_smrt_gps", sep = ",")
    gps_data$GPS_position$files <- basename(gps_file)
  }
  
  # add GPS satellites
  if ("total_satellites" %in% names(gps_raw) &
      "time_sec" %in% names(gps_raw)){
    gps_data$GPS_satellites <- 
      suppressWarnings(
        sens_struct(as.matrix(gps_raw[,c("total_satellites")]),
                    times = gps_raw$time_sec,
                    depid = depid,
                    type = "GPS_satellites")
      )
    gps_data$GPS_satellites$history <- paste(gps_data$GPS_satellites$history, "read_smrt_gps", sep = ",")
    gps_data$GPS_satellites$files <- basename(gps_file)
  }
  
  # add GPS residual
  if ("Residual" %in% names(gps_raw) &
      "time_sec" %in% names(gps_raw)){
    gps_data$GPS_residual <- 
      suppressWarnings(
        sens_struct(as.matrix(gps_raw[,c("Residual")]),
                  times = gps_raw$time_sec,
                  depid = depid,
                  type = "GPS_residual")
      )
    gps_data$GPS_residual$history <- paste(gps_data$GPS_residual$history, "read_smrt_gps", sep = ",")
    gps_data$GPS_residual$files <- basename(gps_file)
  }
  
  # add GPS time error
  if ("TimeError" %in% names(gps_raw) &
       "time_sec" %in% names(gps_raw)){
    gps_data$GPS_time_err <- 
      suppressWarnings(
        sens_struct(as.matrix(gps_raw[,c("TimeError")]),
                    times = gps_raw$time_sec,
                    depid = depid,
                    type = "GPS_time_error")
      )
    gps_data$GPS_time_err$history <- paste(gps_data$GPS_time_err$history, "read_smrt_gps", sep = ",")
    gps_data$GPS_time_err$files <- basename(gps_file)
  }
  
  return(gps_data)
  
} # end of read_smrt_gps()

