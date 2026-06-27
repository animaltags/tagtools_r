#' Get metadata about SM board / DTAG3 / DTAG4 sensor channels
#'
#' SM boards in SMRT tags, and DTAG 3-4 tags, have a system of numeric and string identifiers for different sensors. This function allows conversion between numeric channel IDs and human-readable string IDs.
#' @param ch a vector of sensor numbers or names for which numbers, names, and/or descriptions are required
#' @param sm_dir directory where data files from the SM board or DTAG (e.g., xml and swv files) are stored
#' @param depid Deployment ID string (e.g., "Zica-20260927-12345" or "sw26_123a")

#' @note If no inputs are provided, sm_channels returns metadata about all sensor channels known. If ch is not input but sm_dir and depid are, then information about sensor channels available for deployment depid in tag data files in sm_dir will be read from the data files.
#' @return A data.frame of metadata including variables:
#' 		\itemize{
#' 		\item {ch_names: sensor channel names as strings}
#' 		\item {ch_nums: numeric IDs for each sensor channel}
#' 		\item {description: a description of each channel}
#' 		}
#' @export

sm_channels <- function(ch = NULL,
                        sm_dir = NULL,
                        depid = NULL) {
  # read in metadata about DTAG/SM sensors
  sensor_defs <- utils::read.csv(system.file('extdata', 'd3_sensor_defs.csv', package = 'tagtools'),
                  stringsAsFactors = FALSE)
  # rename variables
  names(sensor_defs) <- gsub(names(sensor_defs), pattern = "name", replacement = "ch_names")
  names(sensor_defs) <- gsub(names(sensor_defs), pattern = "number", replacement = "ch_nums")
  
  if (is.null(ch) && is.null(sm_dir)){
    # if no specific sensors or deployment are specified return EVERYTHING
    return(sensor_defs)
  }
  
  if (is.null(ch) && !is.null(sm_dir) && !is.null(depid)){
    # if inputs are sm_dir and depid, get file name info
    sm_fname_info <- get_sm_fnames(sm_dir, depid)
    if (nrow(sm_fname_info) == 0){
      stop(paste0("No data files for ", depid, " found in ", sm_dir))
    }
    # if there's data, get sensor metadata from xml file
    sm_sensor_config <- get_sm_config(sm_dir, sm_fname_info$file_name[1])
    undoc_sensors <- sm_sensor_config$unique_channels[!(sm_sensor_config$unique_channels %in% sensor_defs$ch_nums)]
    if (length(undoc_sensors) > 0){
      warning(paste0("Tag data includes sensor number(s) not in database which will be missing from sm_channels() output: ", paste0(undoc_sensors, collapse = ", ")))
    }
    sensor_defs <- sensor_defs[sensor_defs$ch_nums %in% sm_sensor_config$unique_channels,]
  }
  
  # if a list of names is input for ch
  # (RARE - these are complicated/detailed names...)
  if ("character" %in% class(ch)){
    sensor_defs <- sensor_defs[sensor_defs$ch_names %in% ch,]
    orphans <- ch[!(ch %in% sensor_defs$ch_names)]
    warning(paste0("Sensor names from ch not found in database: ", paste0(orphans, collapse = ", ")))
  }
  
  # if ch contains a list of numeric channel ID numbers
  if ("numeric" %in% class(ch)){
    sensor_defs <- sensor_defs[sensor_defs$ch_nums %in% ch,]
    orphans <- ch[!(ch %in% sensor_defs$ch_nums)]
    if (length(orphans) > 0){
      warning(paste0("Sensor numbers from ch not found in database: ", paste0(orphans, collapse = ", "))) 
    }
  }

  return(sensor_defs)
} # end of sm_channels

