#' Read SMRT tag data from SM board (.swv and .csv files) and return list of sensor data lists
#'
#' Helper function for \link[tagtools]{read_smrt} - not ordinarily used independently. Read data from SM board (.swv files) and return list of sensor data lists.
#' @param depid string containing the deployment identification code assigned
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param read_sm_swv logical: whether to read data from swv files. Default: TRUE
#' @param read_sm_csv logical: whether to read data from csv files. Default: TRUE
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file).
#' @param recn a numeric vector indicating which swv/csv files should be read. The record numbers are included in file names (last 3 digits), and can be obtained via \code{sm_fnames(sm_dir, depid)}. Default: all files present in sm_dir. This might be used to avoid reading in a long series of data recorded after a tag fell off, for example...but otherwise beware introducing synchronization errors between sensors -- probably best to read all data and then use \code{crop()} later...
#' @param df decimation factor. Default: 1 (no decimation). If a single df value is input, data will be decimated to give a sampling rate for each channel of 1/df of the full original sampling rate. df can also be a vector the same length as ch (or the total number of sensor channels recorded by the SM board as shown by a call to \code{\link{sm_channels}}), if different decimation factors are desired per sensor channel. Decimation is done via \code{\link{decz}} (which calls \code{\link{decdc}}), and includes application of a low-pass anti-alias filter and correction for the group delay of the filter (for "DC accuracy").
#' @param quiet logical; set to TRUE (the default) to suppress messages (from internal helper function \code{\link{sm_assemble_swv}}) about "reading file..." You may want not-quiet operation to monitor progress if many large files are being read, slowly. 
#' @param tz character: the time zone in which time-stamp data in csv file is stored. Default: UTC.
#' @return A list of sensor data structures with all data read from swv and csv data files recorded by the SMRT SM board
#' @export
#' @examples \dontrun{
#' sm_data <- read_smrt_sm(depid, sm_dir)
#' }
read_smrt_sm <- function(depid,
                         sm_dir,
                         read_sm_swv = TRUE,
                         read_sm_csv = TRUE,
                         ch = NULL,
                         recn = NULL,
                         df = 1,
                         tz = "UTC",
                         quiet = TRUE) {
  
  if (read_sm_swv){
    if (!requireNamespace("av", quietly = TRUE)) {
      stop(
        "Package \"av\" must be installed to use this function.",
        call. = FALSE
      )
    }
  }
  
  if (read_sm_csv){
    if (!requireNamespace("vroom", quietly = TRUE)) {
      stop(
        "Package \"vroom\" must be installed to read SMRT csv files",
        call. = FALSE
      )
    }
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
  
  if (read_sm_swv){
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
  }else{ # if !read_sm_swv
    sensor_data <- NULL
  }
  
  if (read_sm_csv){
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
    # figure out start/end index (row) of each file in csv_data0
    csv_meta$start_ix <- 1 + cumsum(c(0, utils::head(csv_meta$nrows, -1)))
    csv_meta$end_ix <- csv_meta$start_ix - 1 + csv_meta$nrows
    
    # preallocate space for output
    csv_data0 <- list2DF(lapply(c(1:ncol(row1)),
                               function(x) vector(mode = "numeric", length = sum(csv_meta$nrows))))
    
    if (header){
      names(csv_data0) <- row1
    }
    
    # read in data from each file and add to csv_data0
    for (f in c(1:nrow(sm_file_info))){
      csv_data0[c(csv_meta$start_ix[f] : csv_meta$end_ix[f]), ] <- 
        vroom::vroom(file = paste0(sm_file_info$sm_dir[f], sm_file_info$file_name[f], ".csv"),
                     col_names = header,
                     col_types = paste0(c("c", rep.int("d", times = ncol(row1) - 1)), collapse = ""),
                     guess_max = 1000,
                     show_col_types = FALSE)
    }
    # convert time stamps to datetimes (doing this in vroom() garbles tz somehow)
    csv_data0[,1] <- lubridate::ymd_hms(csv_data0[,1], tz = tz)
    
    # obtain sampling rate from time stamps
    csv_fs <- 1 / stats::median(as.numeric(diff(csv_data0$Time, units = "sec")), na.rm = TRUE)
    
    # compute time in seconds since tagon including microseconds
    csv_data0$sec_since_start <- 
      as.numeric(difftime(csv_data0$Time,
                          xml_info$recording_start,
                          units = "sec")) +
      csv_data0$Microsecs / 1e6
    
    # interpolate depth data from imperfectly spaced timestamps
    # may want to rethink this in the future b/c unsure whether keeping original is more in sync w/swv data points
    csv_data <- data.frame(sec_since_start = seq(from = 0, by = 1 / csv_fs, to = max(csv_data0$sec_since_start)),
                           depth = NA,
                           dry = NA,
                           temp = NA)
    ivars <- names(csv_data)[names(csv_data) != "sec_since_start"]
    for (v in ivars){
      csv_data[, v] <- stats::approx(x = csv_data0$sec_since_start, 
                              xout = csv_data$sec_since_start,
                              y = csv_data0[, grepl(pattern = v, names(csv_data0), ignore.case = TRUE)],
                              method = "linear",
                              rule = 1, # if data is requested outside measured range value will be NA
                              na.rm = FALSE # interpolate NAs to NA
      )$y
    }
    rm(csv_data0)
  }else{ # if !read_sm_csv
    csv_data <- NULL
  }
  
  message("Converting data to sensor data lists (please be patient...)")
  # make each variable into a sensor data structure (and save )
  # NOTE: need to consider how we keep track of timing and exact start times if any difference between SM board data and csv data.
  if (read_sm_swv){
    # make sensor data lists for each sensor
    swv_sensor_list <- list()
    sensor_names <- data.frame(short = c("ACC", "MAG", "TEMPR"),
                               full = c("acceleration", "magnetometer", "temperature"),
                               nc = c("A", "M", "temperature"))
    for (s in c(1:nrow(sensor_names))){
      my_defs <- sensor_defs[grepl(sensor_defs$ch_names, 
                                   pattern = sensor_names$short[s], 
                                   ignore.case = TRUE), ]
      sensor_axes <- nrow(my_defs)
      sensor_fs <- sensor_data$sampling_rate[names(sensor_data) == my_defs$ch_nums[1]]
      if (nrow(my_defs) > 0){
        if (nrow(my_defs >1)){
          # make sure my_defs is in order x,y,z if multiple axes
          my_defs <- my_defs[order(my_defs$description),]
          swv_sensor_list[[sensor_names$nc[s]]] <- 
            suppressWarnings(
              sens_struct(
                data = matrix(
                  unlist(sensor_data[names(sensor_data) %in% my_defs$ch_nums]),
                  ncol = nrow(my_defs),
                  byrow = FALSE),
                sampling_rate = sensor_fs,
                depid = depid,
                type = sensor_names$full[s],
                name = sensor_names$full[s]) # end of sens_struct
              ) # end of suppress warnings 
        }else{ # if only one axis
          swv_sensor_list[[sensor_names$nc[s]]] <-
            suppressWarnings(
              sens_struct(
                data = matrix(sensor_data[[names(sensor_data) == my_defs$ch_nums]],
                              ncol = 1),
                sampling_rate = sensor_fs,
                depid = depid,
                type = sensor_names$full[s],
                name = sensor_names$full[s])# end of sens_struct 
              )# end of suppress warnings 
        } # end of "if one-axis sensor"
        # record this processing step in the sensor structure's "history" field
        swv_sensor_list[[sensor_names$nc[s]]]$history <- 
          paste(swv_sensor_list[[sensor_names$nc[s]]]$history,
            "read_smrt_sm", sep = ",")
        # which files did data come from?
        swv_sensor_list[[sensor_names$nc[s]]]$files <-
          paste0(basename(swv_fnames), collapse = ", ")
      } # if this sensor is in the sensor_defs
    } # end of loop over s sensors
    # now that we have swv_sensor_list we can erase sensor_data now to avoid multiple huge copies
    rm(sensor_data)
  }else(swv_sensor_list <- NULL) # end of "if read_sm_swv"
  
  if (read_sm_csv){
    csv_sensor_list <- list()
    sensor_names <- data.frame(type = c("press"), # NOTE: could also grab wet/dry and temperature data here if desired
                               name = c("depth"))
    for (s in c(1:nrow(sensor_names))){
      sensor_col <- grepl(pattern = sensor_names$name[s], names(csv_data), ignore.case = TRUE)
      if (any(sensor_col)){
        csv_sensor_list[[sensor_names$name[s]]] <-
          suppressWarnings(
            sens_struct(
              data = matrix(csv_data[, sensor_col],
                            ncol = sum(sensor_col)),
              sampling_rate = csv_fs,
              depid = depid,
              type = sensor_names$type[s],
              name = sensor_names$name[s])# end of sens_struct 
          )# end of suppress warnings
        # record this processing step in the sensor structure's "history" field
        csv_sensor_list[[sensor_names$name[s]]]$history <- 
          paste(csv_sensor_list[[sensor_names$name[s]]]$history,
            "read_smrt_sm", sep = ",")
        # which files did data come from?
        csv_sensor_list[[sensor_names$name[s]]]$files <-
          paste(paste0(sm_file_info$file_name, ".csv"), collapse = ", ")
      } # end of "if this sensor is in the dataset"
    } # end of loop over sensors in csv_data
    # we can now get rid of csv_data
    rm(csv_data)
  }else{csv_sensor_list <- NULL} # end of "if read_sm_csv"
  
  return(c(swv_sensor_list, csv_sensor_list))
} # end of read_smrt_sm()

