#' Read SMRT tag data from SM board (.swv and .csv files) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data from SM board (.swv files) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param info list with metadata about the deployment (used to obtain recording start time)
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file).
#' @return A list of sensor data lists with sensor data, including:
#' 		\itemize{
#' 		\item {A}
#' 		}
#' @export
#' @examples \dontrun{
#' sm_data <- read_smrt_sm(depid, info, sm_dir)
#' }
read_smrt_sm <- function(depid,
                         info,
                         sm_dir,
                         ch = NULL) {
  
  if (!requireNamespace("wav", quietly = TRUE)) {
    stop(
      "Package \"wav\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Input checking
  if (missing(depid) | missing(sm_dir) | missing(info)){
    stop("read_smrt_sm() requires inputs depid, info, and sm_dir")
  }
  
  if (!dir.exists(sm_dir)){
    stop(paste("Folder ", sm_dir, " not found. Please check sm_dir input to read_smrt_sm()." ))
  }
  
  recording_start <- lubridate::ymd_hms(info$deploy_datetime_start,
                                        tz = "UTC")
  
  # get list of swv files
  swv_fnames <- list.files(sm_dir, 
                           full.names = TRUE,
                           pattern = "*.swv")
  
  swv_data <- list()
  for (f in c(1:length(swv_fnames))){
    swv_data[[f]] <- wav::read_wav(swv_fnames[f])
    if (f == 1){
      swv_fs <- attr(swv_data[[f]], "sample_rate")
      swv_bits <- attr(swv_data[[f]], "bit_depth")
    }
  }
  swv_data <- do.call(cbind, swv_data)
  # note that each ROW is one sensor's timeseries
  ###### WORKING HERE JUN 26. Check d3readswv to see how to pull out the sensors of interest.
  # how does it work when the sampling rates are not the same??
  
  # collect metadta from xml file
  xml_info <- get_sm_config(sm_dir)

  # if user has input a subset of channels to read...
  if (!is.null(ch)){
    if ("character" %in% class(ch)){
      channel_meta <- sm_channels(xml_info$unique_channels) ## NEED TO WRITE THIS FUN
      #ch <- ...# convert ch from strings to numbers: get a vector of all the NUMBERS represented in chans corresponding to strings in ch
      # subset xml_info$channels and xml_info$fs to the ones named in cn
    }
  }
  
  if (length(xml_info$unique_channels) == 0){
    stop(paste("No sensor data channels matching ", ch, " found in .swv files in ", sm_dir))
  }
  
  # # WORKING HERE
  # # translating parseswv @ line 125
  # 
  # csv_fnames <- list.files(sm_dir, 
  #                          full.names = TRUE,
  #                          pattern = "*.csv")
  # 
  # archive_fs <- 1 / stats::median(
  #   as.numeric(
  #     difftime(
  #       archive_raw$datetime[100 + (2:n_check)],
  #       archive_raw$datetime[100 + (1:(-1+n_check))],
  #       units = "secs")))
  # 
  # # make list object to hold output sensor data lists
  # archive_data <- list()
  # 
  # # make sensor data lists for each sensor in sensor_var_names
  # for (s in c(1:length(sensor_var_names))){
  #   # if this sensor is in the dataset...
  #   this_sensor_ix <- stringr::str_starts(pattern = tolower(sensor_var_names[s]),
  #                           tolower(names(archive_raw)))
  #   if (sum(this_sensor_ix) > sensor_axes[s]){
  #     warning(paste("Archive data file contains ", sum(this_sensor_ix), 
  #             " variable names containing '",
  #             sensor_var_names[s], "'; using: ",
  #             names(archive_raw)[this_sensor_ix][c(1:sensor_axes[s])],
  #             sep = ""))
  #   }
  #   if (any(this_sensor_ix)){
  #     archive_data[[sensor_corrected_names[s]]] <- 
  #       suppressWarnings(
  #         sens_struct(
  #           data = as.matrix(archive_raw[, names(archive_raw)[this_sensor_ix][c(1:sensor_axes[s])]]),
  #           sampling_rate = archive_fs,
  #           depid = depid,
  #           type = sensor_corrected_names[s],
  #           name = sensor_corrected_names[s]))
  #         archive_data[[sensor_corrected_names[s]]]$history <-
  #           c(archive_data[[sensor_corrected_names[s]]]$history,
  #             "read_smrt_archive")
  #         archive_data[[sensor_corrected_names[s]]]$files <-
  #           basename(archive_file)
  #   }
  # }
  # 
  # return(archive_data)
} # end of read_smrt_archive()

