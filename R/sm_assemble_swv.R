#' Assemble data from a series of swv files into sensor data lists
#'
#' Data from the SM board of SMRT tags is stored in sensor wav files (.swv files). This function calls \code{\link{sm_parse_swv}} to read the data from individual files and uses timing information (obtained via a call to \code{\link{sm_cuetab}}) to assemble them into a continuous timeseries for each sensor, accounting for any recording gaps or timing errors. Data are returned in the form of one sensor data list per sensor processed. Different sensors may be sampled at different sampling rates.
#' @param sm_dir name of directory (including path) where SM data files (.swv and .csv files) are stored
#' @param depid Deployment ID string. Optional, but may be helpful to include if the directory where .swv files are stored contains data from more than one tag deployment.
#' @param df decimation factor. Default: 1 (no decimation). If a single df value is input, data will be decimated to give a sampling rate for each channel of 1/df of the full original sampling rate. df can also be a vector the same length as ch or the number of rows in \code{sensor_defs}, if different decimation factors are desired per sensor channel. Decimation is done via \code{\link{decz}} (which calls \code{\link{decdc}}), and includes application of a low-pass anti-alias filter and correction for the group delay of the filter (for "DC accuracy").
#' @param ch a vector of strings (e.g. 'acc', 'mag', 'pres') or channel numbers indicating which sensor channels to read data from. The channel numbers are the same as those used in the xml metadata files. Default: NULL (read all channels in the .swv file). If \code{sm_file_info} is input, the input \code{ch} should not include channels excluded from \code{sm_file_info}.
#' @param recn a numeric vector indicating which swv/csv files should be read. The record numbers are included in file names (last 3 digits), and can be obtained via \code{sm_fnames(sm_dir, depid)}. Default: all files present in sm_dir. This might be used to avoid reading in a long series of data recorded after a tag fell off, for example...but otherwise beware introducing synchronization errors between sensors -- probably best to read all data and then use \code{crop()} later...If \code{sm_file_info} is input, the input \code{recn} should not include files excluded from \code{sm_file_info}.
#' @param sm_file_info information about swv files in sm_dir. If not input, it is obtained via \code{sm_file_info <- sm_fnames(sm_dir, depid)}
#' @param xml_info metadata about sensor recordings from xml files in sm_dir. If not input, it is obtained via \code{xml_info <- sm_get_config(sm_dir)}.
#' @param sensor_defs metadata about sensors on the tag. If not input, it is obtained via \code{channel_info <- sm_channels(xml_info$unique_channels)}. 
#' @param discard duration of data (in seconds) to discard (and replace with NA) at the start of recording and after each gap in data, to avoid power-up transients. Default: 1 second.
#' @param quiet logical; set to TRUE (the default) to suppress messages about "reading file..." You may want not-quiet operation to monitor progress if many large files are being read, slowly. 

#' @note This function is analogous in function to d3readswv from the DTAG Matlab tool kit. d3readswv() also has an option to return only sampling rate and sensor channel names instead of sensor data; for this function use \link{sm_get_config} and perhaps \link{sm_channels}.
#' @return A list containing one sensor data list for each sensor in \code{ch}, plus a list item named sampling_rate with the final sampling rate of each sensor
#' @export

sm_assemble_swv <- function(sm_dir,
                            depid,
                            df = 1,
                            ch = NULL,
                            recn = NULL,
                            sm_file_info = sm_fnames(sm_dir, depid),
                            xml_info = sm_get_config(sm_dir),
                            sensor_defs = sm_channels(xml_info$unique_channels),
                            discard = 1,
                            quiet = TRUE
                            ){
# make sure required package for reading wav files is installed
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function to read sensor wav (swv) files.",
      call. = FALSE
    )
  }
  
  # check format of sm_dir input
  sm_dir <- sm_dir_check(sm_dir)
  
  # get cue table with info about timing of swv files
  swv_cues0 <- sm_cuetab(sm_dir, suffix = "swv")
  
  # restrict sm_file_info and swv_cues0 to just requested file numbers, if recn is given
  if (!is.null(recn)){
    recn <- sort(recn)
    if (any(diff(recn) > 1)){
      warning("Non-consecutive data files in recn; be sure you want to concatenate them together!")
    }
    sm_file_info <- sm_file_info[sm_file_info$recn %in% recn,]
    swv_cues0 <- swv_cues[swv_cues0$recn %in% recn, ]
  }
  
  # if ch input, restrict sensor_defs and xml_info accordingly
  if (!is.null(ch)){
    sensor_defs <- sm_ch_subset(sensor_defs, ch)
    xml_info$unique_channels <- xml_info$unique_channels[xml_info$unique_channels %in% sensor_defs$ch_nums]
    xml_info$sampling_rate <- xml_info$sampling_rate[xml_info$unique_channels %in% sensor_defs$ch_nums]
  }
  
  # add sampling rate to sensor_defs
  sensor_defs <- merge(sensor_defs, data.frame(ch_nums = xml_info$unique_channels,
                                               sampling_rate = xml_info$sampling_rate))
  # sampling rate multiplier
  # (some sensors use multiple channels in swv file to achieve higher sampling rates)
  sensor_defs$fsmult <- round(sensor_defs$sampling_rate / xml_info$fb)
  
  # warn user if pressure but not temp requested
  # (this would be bad for DTAGs which use it for press-temp cal...)
  # (but for SMRT tags the pressure data is black-box pre-calibrated)
  # warning currently suppressed b/c no way to tell from xml_info that it's a SMRT vs DTAG4
  # if (grepl(pattern = 'PRES', sensor_defs$ch_names) &
  #     !grepl(pattern = 'TEMP', sensor_defs$ch_names)){
  #    warning("Selected sensors include pressure, but not temperature; consider whether temperature is also needed for pressure-sensor calibration.")
  # }
  
  # decimation factor(s)
  if (any(df > 1)){
    if (any(diff(df) != 1)){
      if (length(df) != nrow(sensor_defs)){
        warning("Length of input df does not match the number of sensor channels: using only the first entry of df.")
        df <- rep.int(df[1], nrow(sensor_defs))
      } # else if df has right # of multiple different entries keep it as-is
    }else{
      # if df has multiple identical entries make sure it's the right length
      df <- rep.int(df[1], nrow(sensor_defs))
    }
  }else{
    # if df is 1 make it a vector of 1s same length as # sensors
    df <- rep.int(1, nrow(sensor_defs))
  }
  
  # collapse cuetab to amalgamate data-filled blocks ( = ones with status = 1 or 0, not -1)
  # this could be done w/dplyr but keeping the matlab-y loop b/c the number of rows in cuetab are unlikely to be big enough to make it slow
  swv_cues <- swv_cues0[1,]
  if (nrow(swv_cues0) > 1){ # for tags with only one block (it better be data!)
  for (k in c(2:nrow(swv_cues0))){
    if (swv_cues0$status[k] < 0 || # if this block is a gap OR
        swv_cues0$recn[k] != swv_cues$recn[nrow(swv_cues)] || # if this block is from a new recn OR
        swv_cues$status[nrow(swv_cues)] < 0 # if the current last row of swv_cues is a gap
    ){
      # if conditions are met add kth row of cues0 to cues
      swv_cues <- rbind(swv_cues, swv_cues0[k, ])
    }else{
      # otherwise, it's data or a filled gap in the same recn so ADD its samples to the existing last row of swv_cues
      swv_cues$n_samples[nrow(swv_cues)] <- swv_cues$n_samples[nrow(swv_cues)] + swv_cues0$n_samples[k] 
    }
  }
  }

  # read in swv data block by block
  dodiscard <- ifelse(discard > 0, TRUE, FALSE)
  ssamp <- 1 # sample in current swv to start reading
  osamp <- rep(1, nrow(sensor_defs)) # index of location to paste in first sample from current block, in full-rate output
  odsamp <- rep(1, nrow(sensor_defs)) # index of location to paste in first sample from current DECIMATED block
  cues_row <- 1
  file_samps <- 0
  # pre allocate space for output: total size = total samples (from cuetab) * fsmult / df
  # note: sensor data vectors will start out zero-filled.
  sensor_data <- lapply(
    c(1:nrow(sensor_defs)), 
    function(x) vector(mode = "numeric", 
                       length = sum(swv_cues$n_samples) * sensor_defs$fsmult[x] / df[x]))
  

  while(cues_row <= nrow(swv_cues)){
    blk <- swv_cues[cues_row, ]
    if (blk$status < 0){ # if this is a gap block
      message(paste0("gap filled in recn ", blk$recn, " of ", blk$n_samples, " samples"))
      # generate NAs to fill gap blocks. block_data is a list of length = nrow(sensor_defs)
      # each element is a vector of blk$n_samples NAs
      block_data <- lapply(
        c(1:nrow(sensor_defs)),
        function(channel) NA * vector(mode = "numeric",
                                      length = blk$n_samples * sensor_defs$fsmult[channel]))
      dodiscard <- ifelse(discard > 0, TRUE, FALSE) # do a discard on the next block
      # ssamp[2] <- blk$n_samples
    }else{ # end of "if it's a gap block"...now "if it's data or 0-filled gap"
      if (!quiet){
        if (ssamp == 1){
          message(paste0("Reading file ", sm_file_info$file_name[blk$recn]))
        }else{
          message(paste0("Reading more from file ", sm_file_info$file_name[blk$recn]))
        }
      }# end of messages about files being read
      
      # read in this block of data 
      this_file_info <- sm_file_info[sm_file_info$recn == blk$recn,]
      this_swv <- paste0(this_file_info$sm_dir, this_file_info$file_name)
      this_swv_meta <- av::av_media_info(paste0(this_swv, ".swv"))
      block_data <- sm_parse_swv(
        swv_file = this_swv,
        ch = ch,
        start_samp = ssamp,
        end_samp = ssamp + blk$n_samples-1
      )
      
      # I don't know why this is -- return empty data if there was none??
      if (length(block_data$sampling_rate) == 0){return(sensor_data)}
      
      # if discard > 0...and if this is the first time on this file or block...
      if (dodiscard){
        # number of samples in discard seconds of data
        nfill <- round(xml_info$fb * discard)
        for (sens in c(1:nrow(sensor_defs))){
          # fill in first discard secs of each sensor data with NA
          fill <- NA * vector(mode = "numeric", length = nfill * sensor_defs$fsmult[sens])
          block_data$data[[sens]][c(1:length(fill))] <- fill
        }
        dodiscard <- FALSE # don't do this more than once per block
      }
      
      # remove single-sample outliers in data from sensors that are NOT acc or mag
      for (sens in c(1:nrow(sensor_defs))){
        if (!grepl(pattern = "acc", sensor_defs$ch_names[sens], ignore.case = TRUE) &
            !grepl(pattern = "mag", sensor_defs$ch_names[sens], ignore.case = TRUE)){
          block_data$data[[sens]] <- deglitch(block_data$data[[sens]])
        }
      }
      # to match na-fill blocks, we need just the list of sensor data vectors
      # block_data$data and not anything else in block_data from sm_parse_swv
      block_data <- block_data$data
      # this is INSIDE "if it's data" b/c the unfilled missing blocks WILL NOT count toward the swv-file-dur total n sampls
      file_samps <- file_samps + length(block_data[[1]]) / sensor_defs$fsmult[1]
    } # end of "if it's a data block"

    # decimate sensor data as required and then move it from block_data to sensor_data
    # initial decimation "filter state" is NULL
    # for later data blocks it will be passed on from the prev iteration
    if (any(df > 1)){
      dec_filter_state <- lapply(c(1:nrow(sensor_defs)), function(x) NULL)
    }
    nsamps <- length(block_data[[1]]) / sensor_defs$fsmult[1]

    
    for (s in c(1:nrow(sensor_defs))){
      if (df[s] > 1){
        dec_data <- decz(block_data[[s]], df = df[s], Z = dec_filter_state[[s]])
        dec_filter_state[[s]] <- dec_data$Z
        sensor_data[[s]][c(odsamp[s] : (odsamp[s] + length(dec_data$y) - 1))] <- dec_data$y
        odsamp[s] <- odsamp[s] + length(dec_data$y)
      }else{
        sensor_data[[s]][c(osamp[s] : (osamp[s] + length(block_data[[s]]) - 1))] <- block_data[[s]]
        osamp[s] <- osamp[s] + length(block_data[[s]])
      }
    }
    
    # if there's more to read FROM THIS FILE this is where to start next time
    ssamp <- ssamp + nsamps
    
    # reset counters (unless we have just read the last block)
    if (cues_row < nrow(swv_cues)){
      # if the next block will be from a new swv (according to swv_cues recn values)...
      if (swv_cues$recn[cues_row] != swv_cues$recn[1 + cues_row]){
        # then we should also have read in ALL THE DATA from the current swv file
        if (file_samps >= round(this_swv_meta$duration * this_swv_meta$audio$sample_rate)){
          # if both conditions are met we will read from a new file next so reset ssamp, file_samps
          ssamp <- 1
          file_samps <- 0
        }else{
          warning(paste0("File ", sm_file_info$file_name[sm_file_info$recn == swv_cues$recn[cues_row]],
                      " contains ", round(this_swv_meta$duration * this_swv_meta$audio$sample_rate),
                      " total samples, but only ", file_samps,
                      " have been read. If this discrepancy is not expected, check cue table."))
        } 
      }
    }
    
    # if all samples have been read from this cuetab row, move to the next one
    # unlike d3readswv, sm_parse_swv will never read in less that the requested samples
    # so one CUE ROW should never actually need multiple iterations - this condition should always be TRUE
    if (nsamps >= swv_cues$n_samples[cues_row]){
      cues_row <- cues_row + 1
    }else{
      stop(paste0("Error reading swv data: expected ", 
                  swv_cues$n_samples[cues_row], 
                  " samples from file ",
                  sm_file_info$file_name[sm_file_info$recn == swv_cues$recn[cues_row]],
                  ", but only got ", nsamps))
    }
  } # end of while loop over cuetab rows
  
  # if decimation was done
  if (any(df > 1)){
    # get the last few samples out of the decimation filter
    for (s in c(1:nrow(sensor_defs))){
      if (df[s] > 1){
        last_dec_output <- decz(x = NULL, df = df[s], Z = dec_filter_state[[s]])
        sensor_data[[s]][c(odsamp[s] : (odsamp[s] + length(last_dec_output$y) - 1))] <- last_dec_output$y
        odsamp[s] <- odsamp[s] + length(last_dec_output$y)
      }
    }
  }
  
  # matlab code here makes sure every data vector is a column vector but we don't bother...may want to ensure this when converting to sensor structures tho
  # name elements of sensor_data with the channel ID numbers
  names(sensor_data) <- as.character(sensor_defs$ch_nums)
  # store final sampling rates
  sensor_data$sampling_rate <- sensor_defs$sampling_rate / df
  return(sensor_data)
} # end of sm_assemble_swv

