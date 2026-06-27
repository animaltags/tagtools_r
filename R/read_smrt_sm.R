#' Read SMRT tag data from SM board (.swv and .csv files) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data from SM board (.swv files) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param info list with metadata about the deployment (used to obtain recording start time)
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file).
#' @param recn a numeric vector indicating which swv/csv files should be read. The record numbers are included in file names (last 3 digits), and can be obtained via \code{get_sm_fnames(sm_dir, depid)}. Default: all files present in sm_dir. This might be used to avoid reading in a long series of data recorded after a tag fell off, for example...but otherwise beware introducing synchronization errors between sensors -- probably best to read all data and then use \code{crop()} later...
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
                         ch = NULL,
                         recn = NULL) {
  
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
  
  # make sure sm_dir ends with / (and uses only / not \, for mac compatibility)
  if (!missing(sm_dir)){
    if (!stringr::str_ends(sm_dir, pattern = stringr::fixed("/"))){
      sm_dir <- paste0(sm_dir, "/")
    }
    sm_dir <- gsub(sm_dir, pattern = "\\", replacement = "/", fixed = TRUE)
  }
  
  if (!missing(sm_dir) & !dir.exists(sm_dir)){
    stop(paste("Folder ", sm_dir, " not found. Please check sm_dir input to get_sm_fnames()." ))
  }
  
  if (!dir.exists(sm_dir)){
    stop(paste("Folder ", sm_dir, " not found. Please check sm_dir input to read_smrt_sm()." ))
  }
  
  # collect metadta from xml file
  xml_info <- get_sm_config(sm_dir)
  
  # if user has input a subset of channels to read...
  # and they are character...
  if (!is.null(ch)){
    channel_meta <- sm_channels(xml_info$unique_channels) 
    if ("character" %in% class(ch)){
      # subset the tag's available channels to include just the ones requested
      # by matching the NAMES
      channel_meta <- 
        channel_meta[grepl(pattern = paste0(ch, collapse = "|"), 
                           channel_meta$ch_names, 
                           ignore.case = TRUE),]
    }
    # and if ch are numeric...
    if ("numeric" %in% class(ch)){
      # subset the tag's available channels to include just the ones requested
      # by matching the NUMBERS
      channel_meta <-
        channel_meta[channel_meta$ch_nums %in% ch, ]
    }
    if (nrow(channel_meta) == 0){
      stop(paste("No sensor data channels matching ", ch, " found in .swv files in ", sm_dir))
    }
  }
  # at this point channel_meta has metadata about either all the sensors in the data files,
  # or the subset the user has requested to read in.
  
  # get list of swv files
  sm_file_meta <- get_sm_fnames(sm_dir, depid)
  
  if (!is.null(recn)){
    recn <- sort(recn)
    if (any(diff(recn) > 1)){
      warning("Non-consecutive data files in recn; be sure you want to concatenate them together!")
    }
    sm_file_meta <- sm_file_meta[sm_file_meta$recn %in% recn,]
  }
  swv_fnames <- paste0(sm_dir, sm_file_meta$file_name, ".swv")
  
  for (f in c(1:length(swv_fnames))){
    if (!file.exists(swv_fnames[f])){
      warning(paste0("File ", basename(swv_fnames[f]), " not found in ", sm_dir))
    }
  }
  
  # WORKING HERE
  # # translating parseswv @ line 125
  # we want to only read in, or at least only keep, the channels named in channel_meta
  swv_data <- list()
  for (f in c(1:length(swv_fnames))){
    # to be parallel w/matlab dtag tools we would include an option to read in
    # only PART of the SWV file (certain samples) but that's not as easy in R.
    # wav::read_wav() cannot read just part of a file. so it might mean reading all and then junking some.
    # here we need to write a separate per-swv-file function instead of just reading the files in a loop.
    swv_data[[f]] <- wav::read_wav(swv_fnames[f])
    if (f == 1){
      swv_fs <- attr(swv_data[[f]], "sample_rate")
      swv_bits <- attr(swv_data[[f]], "bit_depth")
    }
    # the per-wav-file function will also need to do the stuff around line 163+ in d3parseswv
  }
  
  # the per-swv-file function will need to be called by an analogue of d3readswv that will put them together and deal with timing etc.
  # it should also have an option to decimate data if desired

  
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

