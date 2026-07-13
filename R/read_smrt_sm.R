#' Read SMRT tag data from SM board (.swv and .csv files) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data from SM board (.swv files) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file).
#' @param recn a numeric vector indicating which swv/csv files should be read. The record numbers are included in file names (last 3 digits), and can be obtained via \code{sm_fnames(sm_dir, depid)}. Default: all files present in sm_dir. This might be used to avoid reading in a long series of data recorded after a tag fell off, for example...but otherwise beware introducing synchronization errors between sensors -- probably best to read all data and then use \code{crop()} later...
#' @param df decimation factor. Default: 1 (no decimation). If a single df value is input, data will be decimated to give a sampling rate for each channel of 1/df of the full original sampling rate. df can also be a vector the same length as ch (or the total number of sensor channels recorded by the SM board as shown by a call to \code{\link{sm_channels}}), if different decimation factors are desired per sensor channel. Decimation is done via \code{\link{decz}} (which calls \code{\link{decdc}}), and includes application of a low-pass anti-alias filter and correction for the group delay of the filter (for "DC accuracy").
#' @param quiet logical; set to TRUE (the default) to suppress messages (from internal helper function \code{\link{sm_assemble_swv}}) about "reading file..." You may want not-quiet operation to monitor progress if many large files are being read, slowly. 
#' @param tz character: the time zone in which time-stamp data in csv file is stored. Default: UTC.
#' @return A list including:
#' 		\itemize{
#' 		\item swv_data, a list with one element per sensor channel in ch. (A data.frame is not used because sensors may be sampled at different rates.) The list items are named using the sensor ID numbers in ch. There is also an additional (last) list item, \code{sampling_rate}, which is a vector of sampling rates for the channels in ch. 
#' 		\item csv_data, a data.frame with data from the csv files found in sm_dir. (This is generally data from the WC board recorded by the SM board such as the depth and wet-dry sensors.)
#' 		}
#' @export
#' @examples \dontrun{
#' sm_data <- read_smrt_sm(depid, info, sm_dir)
#' }
read_smrt_sm <- function(depid,
                         sm_dir,
                         ch = NULL,
                         recn = NULL,
                         df = 1,
                         tz = "UTC",
                         quiet = TRUE) {
  
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop(
      "Package \"vroom\" must be installed to read SMRT csv files",
      call. = FALSE
    )
  }
  
  # Input checking
  if (missing(depid) | missing(sm_dir)){
    stop("read_smrt_sm() requires inputs depid, info, and sm_dir")
  }
  
  sm_dir <- sm_dir_check(sm_dir)
  
  # collect metadta from xml file
  xml_info <- sm_get_config(sm_dir)
  
  # if user has input a subset of channels to read...
  # and they are character...
  sensor_defs <- sm_channels(xml_info$unique_channels)
  if (!is.null(ch)){
    sensor_defs <- sm_ch_subset(sensor_defs, ch)
  }
  # at this point sensor_defs has metadata about either all the sensors in the data files,
  # or the subset the user has requested to read in.
  
  # get list of swv files
  sm_file_info <- sm_fnames(sm_dir, depid)
  
  if (!is.null(recn)){
    recn <- sort(recn)
    if (any(diff(recn) > 1)){
      warning("Non-consecutive data files in recn; be sure you want to concatenate them together!")
    }
    sm_file_info <- sm_file_info[sm_file_info$recn %in% recn,]
  }
  swv_fnames <- paste0(sm_dir, sm_file_info$file_name, ".swv")
  
  for (f in c(1:length(swv_fnames))){
    if (!file.exists(swv_fnames[f])){
      warning(paste0("File ", basename(swv_fnames[f]), " not found in ", sm_dir))
    }
  }

  sensor_data <- sm_assemble_swv(sm_dir = sm_dir,
                                 depid = depid,
                                 ch = ch,
                                 recn = recn,
                                 sm_file_info = sm_file_info,
                                 xml_info = xml_info,
                                 sensor_defs = sensor_defs,
                                 df = df,
                                 quiet = quiet)
  if (!sum(sapply(paste0(sm_file_info$sm_dir, sm_file_info$file_name, ".csv"), FUN = file.exists))){
    stop(paste0("No csv files with names like ",
                sm_file_info$file_name[1],
                " found in ", sm_dir))
  }
  
  row1 <- vroom::vroom(file = paste0(sm_file_info$sm_dir, sm_file_info$file_name, ".csv"),
                       n_max = 1,
                       col_names = FALSE,
                       show_col_types = FALSE,
                       progress = FALSE)[1,]
  if (sum(apply(row1[c(2:ncol(row1)),], MARGIN = 2, FUN = is.character)) > 0){
    # if there's a header row on the files
    header = TRUE
  }else{
    # if there's no header row
    header = FALSE
  }
  
  # get nrows (of DATA not counting col names) per file (without reading in the data)
  csv_meta <- 
    data.frame(nrows = sapply(paste0(sm_file_info$sm_dir, sm_file_info$file_name, ".csv"),
                              function(x) length(vroom::vroom_lines(x, altrep = TRUE, progress = FALSE)) - 1*as.numeric(header),
                              USE.NAMES = FALSE))
  # figure out start/end index (row) of each file in csv_data
  csv_meta$start_ix <- 1 + cumsum(c(0, utils::head(csv_meta$nrows, -1)))
  csv_meta$end_ix <- csv_meta$start_ix - 1 + csv_meta$nrows
  
  # preallocate space for output
  csv_data <- list2DF(lapply(c(1:ncol(row1)),
                     function(x) vector(mode = "numeric", length = sum(csv_meta$nrows))))
  
  if (header){
    names(csv_data) <- row1
  }
  
  # read in data from each file and add to csv_data
  for (f in c(1:nrow(sm_file_info))){
    csv_data[c(csv_meta$start_ix[f] : csv_meta$end_ix[f]), ] <- 
      vroom::vroom(file = paste0(sm_file_info$sm_dir[f], sm_file_info$file_name[f], ".csv"),
                   col_names = header,
                   col_types = paste0(c("c", rep.int("d", times = ncol(row1) - 1)), collapse = ""),
                   guess_max = 1000,
                   show_col_types = FALSE)
  }
  # convert time stamps to datetimes (doing this in vroom() garbles tz somehow)
  csv_data[,1] <- lubridate::ymd_hms(csv_data[,1], tz = tz)
  
  # obtain sampling rate from time stamps
  csv_fs <- 1 / stats::median(as.numeric(diff(csv_data$Time, units = "sec")), na.rm = TRUE)
  
  # compute time in seconds since tagon including microseconds
  csv_data$sec_since_start <- 
    as.numeric(difftime(csv_data$Time,
                        xml_info$recording_start,
                        units = "sec")) +
    csv_data$Microsecs / 1e6
  
  return(list(swv_data = sensor_data, csv_data = csv_data))
  # make each variable into a sensor data structure (and save )
  # NOTE: need to consider how we keep track of timing and exact start times if any difference between SM board data and csv data.

  # 
  # # make sensor data lists for each sensor
  # note that for A and M this will have to grab all three axes and keep them in order xyz
  # the col "ch_names" and "cal" and "description" will contain acc or mag, and qualifier1 and description and comment will contain letter x/y/z. description and comment will contain "x/y/z axis"
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

