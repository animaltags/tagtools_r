#' Read SMRT tag data from SM board (.swv and .csv files) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data from SM board (.swv files) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file).
#' @param recn a numeric vector indicating which swv/csv files should be read. The record numbers are included in file names (last 3 digits), and can be obtained via \code{sm_fnames(sm_dir, depid)}. Default: all files present in sm_dir. This might be used to avoid reading in a long series of data recorded after a tag fell off, for example...but otherwise beware introducing synchronization errors between sensors -- probably best to read all data and then use \code{crop()} later...
#' @param df decimation factor. Default: 1 (no decimation). If a single df value is input, data will be decimated to give a sampling rate for each channel of 1/df of the full original sampling rate. df can also be a vector the same length as ch (or the total number of sensor channels recorded by the SM board as shown by a call to \code{\link{sm_channels}}), if different decimation factors are desired per sensor channel. Decimation is done via \code{\link{decz}} (which calls \code{\link{decdc}}), and includes application of a low-pass anti-alias filter and correction for the group delay of the filter (for "DC accuracy").
#' @param quiet logical; set to TRUE (the default) to suppress messages (from internal helper function \code{\link{sm_assemble_swv}}) about "reading file..." You may want not-quiet operation to monitor progress if many large files are being read, slowly. 
#' @return A list of sensor data lists with sensor data, including:
#' 		\itemize{
#' 		\item {A}
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
                         quiet = TRUE) {
  
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function.",
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
  
  # WORKING HERE 7/10
  # Also need to read the data from the WC board recorded by SM board (in csv files)
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

