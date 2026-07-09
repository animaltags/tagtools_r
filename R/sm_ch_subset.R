#' Subset sensor_defs data.frame to include only sensor channels in ch
#'
#' This is a utility function called by other functions that work with SMRT data (not usually called directly by users)
#' @param sensor_defs metadata about sensors on the tag (likely obtained by a call to \code{\link{sm_channels}}). 
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. If no \code{ch} is input, this function will simply return the input \code{sensor_defs}.


#' @return The \code{sensor_defs} data.frame, subsetted to include only the channels in \code{ch}.
#' @export

sm_ch_subset <- function(sensor_defs,
                         ch){
  if (missing(ch)){
    return(sensor_defs)
  }
  
  # if user has input a subset of channels to read...
  # and they are character...
  if ("character" %in% class(ch)){
    # subset the tag's available channels to include just the ones requested
    # by matching the NAMES
    sensor_defs <- 
      sensor_defs[grepl(pattern = paste0(ch, collapse = "|"), 
                        sensor_defs$ch_names, 
                        ignore.case = TRUE),]
  }
  
  # and if ch are numeric...
  if ("numeric" %in% class(ch)){
    # subset the tag's available channels to include just the ones requested
    # by matching the NUMBERS
    sensor_defs <-
      sensor_defs[sensor_defs$ch_nums %in% ch, ]
  }
  
  # check there is some output left
  if (nrow(sensor_defs) == 0){
    stop(paste("No sensor data channels matching ", ch, " found in sensor_defs data.frame"))
  }

  return(sensor_defs)
}

