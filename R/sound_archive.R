#' Create a "sound archive" (SA) sensor data list with metadata about acoustic recordings from a SMRT or DTAG deployment.
#'
#' SMRT tags and DTAGs record acoustic data in wav files. These files are too large to include inside a netCDF file. However, we can include metadata about the recordings (file names, duration, sampling rate, and information about any gaps in recording) in a "SA" ("sound archive") list for inclusion in the netCDF archive. This function creates the SA list, which can also be used to figure out where (which file, and when within the file) to find a sound clip at a particular time.
#' @param sm_dir directory where xml file(s) are stored
#' @param depid deployment ID string
#' @param err_thr_sec threshold in seconds for reporting timing errors (that also exceed err_thr_samp). Default: 0.005 sec.
#' @param err_thr_samp threshold in samples for reporting timing errors (that also exceed err_thr_sec). Default: 10 samples.
#' @param suffix file extension of the files to catalog. They must be in a wav-format configuration. Default: 'wav' (could also be 'swv' or some other file extension assigned in the future to files in wav format).

#' @return A data.frame with metadata about acoustic wav files associated with the tag deployment. Variables include:
#' 		\itemize{
#' 		\item {recn: file number}
#' 		\item {start_sec: time of first sample in block, in seconds since recording start}
#' 		\item {n_samples: number of samples in the block}
#' 		\item {status: status of the block. 0 means data; -1 means an unfilled gap; 1 means a zero-filled gap.}
#' 		}
#' @export

sound_archive <- function(sm_dir,
                          depid,
                          err_thr_sec = 0.005,
                          err_thr_samp = 10,
                          suffix = 'wav'){
  
  if (missing(sm_dir) || missing(depid)){
    stop("Both inputs sm_dir and depid are required to create a sound archive")
  }
  
  # make sure required package for reading wav files is installed
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function to read wav files.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" (for reading xml files) must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # format sm_dir: final /, using / not \ in path
  sm_dir <- check_sm_dir(sm_dir)
  
  # get data.frame with file_name, sm_dir, recn, device_serial, device_id
  sm_fnames <- get_sm_fnames(sm_dir, depid)
  xml_file <- paste0(sm_dir, sm_fnames$file_name[1], ".xml")
  
  # get metadata from xml file
  xml_info <- get_sm_config(xml_file = xml_file)
  # note: xml_info$afs is acous sampling rate
  
  # get wav block info corresponding to each wav file
  check_xml_wavblk <- TRUE
  cuetab <- list() # first a list of data frames, to be rbind-ed later
  for (k in c(1:nrow(sm_fnames))){
    cuetab[[k]] <- data.frame() # cuetab is obtained/used separately for each file (not ultimately output)
    if (check_xml_wavblk){
      this_doc <- xml2::read_xml(paste0(sm_dir, sm_fnames$file_name[k], ".xml"))
      # note: this code not tested b/c the SMRT xml per-.dtg files do not have WAVBLKs
      if ("WAVBLK" %in% xml2::xml_name(xml2::xml_children(this_doc))){
        if (!is.na(xml2::xml_find_first(xml_info$xml_doc, "SUFFIX"))){
          if (grepl(pattern = suffix, xml2::xml_find_first(this_doc, "SUFFIX") |> xml2::xml_text())){
            if (is.na(xml2::xml_find_first(this_doc, "RTIME")) |
                is.na(xml2::xml_find_first(this_doc, "MTICKS")) | 
                is.na(xml2::xml_find_first(this_doc, "NSAMPS"))){
              cuetab[[k]] <- rbind(cuetab[[k]], data.frame(RTIME = as.numeric(xml2::xml_find_all(this_doc, "RTIME") |> xml2::xml_double()),
                                             MTICKS = as.numeric(xml2::xml_find_all(this_doc, "MTICKS") |> xml2::xml_double()),
                                             NSAMPS = as.numeric(xml2::xml_find_all(this_doc, "NSAMPS") |> xml2::xml_double()),
                                             STATUS = 0,
                                             SUFFIX = suffix))
            } # end of "if the needed wavblk info is there"
          } # end of "if SUFFIX matches input suffix"
        } # end of "if xml doc has a SUFFIX entry"
      }else{
        # if there was no wavblk info in the first xml file, don't bother to check later ones
        check_xml_wavblk <- FALSE 
      } # end of "if xml doc has WAVBLK entry"
    } # end of "if check_xml_wavblk"
    
    # if xml file did not have WAVBLK entries then check for wavt files
    wavt_file <- paste0(sm_dir, sm_fnames$file_name[k], ".wavt")
    if (file.exists(wavt_file)){
      wavt_data <- utils::read.csv(wavt_file)
      cuetab[[k]] <- rbind(cuetab[[k]], wavt_data[wavt_data$SUFFIX == suffix,])
    }else{
      # apparently there is an "old" style of timing files that might have extension .swvt
      if (file.exists(paste0(sm_dir, sm_fnames$file_name[k], ".", suffix, "t"))){
        suffixt_data <- utils::read.csv(paste0(sm_dir, sm_fnames$file_name[k], ".", suffix, "t"))
        cuetab[[k]] <- rbind(cuetab[[k]], suffixt_data[suffixt_data$SUFFIX == suffix, ])
      }
    }
    
    # get sample rate & count from kth wav file & check consistency with xml / cuetab
    wav_file <- paste0(sm_dir, sm_fnames$file_name[k], '.', suffix)
    if (!file.exists(wav_file)){
      warning(paste0('No ', suffix, ' file found for recording ',  sm_fnames$file_name[k], ' - skipping...'))
    }else{
      # as long as the kth wav (or swv) file exists...
      # get acoustic recording metadata
      wav_info <- av::av_media_info(wav_file)
      # check that wav file and xml file sampling rates agree
      if (wav_info$audio$sample_rate != xml_info$afs0){
        warning(paste0('Sampling rate mismatch in recording: ', sm_fnames$file_name[k]))
      }
      if (nrow(cuetab[[k]]) > 0){
        # check that total samples info from wav file and wavblks are same
        if (round(as.numeric(wav_info$duration * wav_info$audio$sample_rate)) !=
            as.numeric(sum(cuetab[[k]]$NSAMPS))){
          warning(paste0('Sample count mismatch in recording: ', sm_fnames$file_name[k]))
        }
      }else{
        # some DTAG3 xml files made with an old version of d3read don't have WAVBLK fields
        warning("No WAVBLK fields found in xml files - check version of d3read and re-run?")
        if (!check_xml_wavblk){
          this_doc <- xml2::read_xml(paste0(sm_dir, sm_fnames$file_name[k], ".xml"))
          this_cue <- xml2::xml_find_all(this_doc, "CUE")
          # if this_cue has attr SUFFIX and it matches the input suffix...
          if (suffix %in% (xml2::xml_find_first(this_cue, "@SUFFIX") |> xml2::as_list() |> unlist())){
            for (c in c(1:length(this_cue))){
              if (grepl(pattern = suffix, xml2::xml_find_all(this_cue[c], "@SUFFIX") |> xml2::xml_text())){
                cuetab[[k]] <- data.frame(RTIME = lubridate::ymd_hms(xml2::xml_attr(this_cue[c], "TIME")),
                                   MTICKS = this_cue[c] |> xml2::xml_double() * 1e6,
                                   NSAMPS = wav_info$duration * wav_info$audio$sample_rate,
                                   STATUS = 0,
                                   SUFFIX = suffix)
              } # end of "if SUFFIX matches input suffix"
            } # end of loop over entries of this_cue
          } #end of "if this_cue has attr "SUFFIX"
        } # end of "if xml files didn't have wavblk info"
      } # end of ifelse "we have/don't have cuetab now"
    } # end of extracting info from the kth wav file
    # record in cuetab which file the wavblk info corresponds to
    cuetab[[k]]$RECN <- k
  }# end loop over wav/xml/wavt data files
  cuetab <- do.call(rbind, cuetab)
  
  if (is.null(xml_info$afs)){
    warning("Warning: Unable to determine sampling rate for this configuration")
  }
  
  # if cuetab has at least 2 rows, check the timing
  if (nrow(cuetab) > 1){
    # matlab cuetab cols: [recn, RTIME, MTICKS, NSAMPS, STATUS] 
    frst <- 1
    overrun <- 0
    while (1){
      tpred <- cumsum(as.numeric(cuetab$NSAMPS[c(1:(nrow(cuetab) - 1))])) / xml_info$afs
      tnxt <- (cuetab$RTIME[c(2:nrow(cuetab))] - cuetab$RTIME[1]) + (cuetab$MTICKS[c(2:nrow(cuetab))] - cuetab$MTICKS[1])*1e-6
      terr <- tnxt - tpred
      serr <- round(terr * xml_info$afs)
      err_ix <- which(terr > err_thr_sec & serr > err_thr_samp)
      if (length(err_ix) == 0){
        cuetab$MTICKS[c(2:nrow(cuetab))] <- cuetab$MTICKS[c(2:nrow(cuetab))] - terr * 1e6
        break
      }
      err_ix <- err_ix[1] # we only take care of one timing error at a time starting with the first one
      cuetab$MTICKS[c(2:err_ix)] <- cuetab$MTICKS[c(2:err_ix)] - terr[c(1:(err_ix - 1))] * 1e6
      if (frst){
        warning("Gaps found between data blocks. Gaps are allowed and are managed by tagtools, but if gaps are unexpected check version of d3read or d4read used to unpack dtg files.")
        frst <- 0
      }
      if (err_ix < nrow(cuetab) && cuetab$RECN[err_ix] == cuetab$RECN[err_ix + 1]){
        message(paste0('Gap in file ', sm_fnames$file_name[err_ix], 'of ', 
                       round(terr[err_ix], digits = 3), ' seconds (',
                       serr[err_ix], ' samples).'))
      }else{
        message(paste0('Gap between files ', sm_fnames$file_name[err_ix],
                       ' and ', sm_fnames$file_name[err_ix + 1], ' of ',
                       round(terr[err_ix], digits = 3), ' seconds (',
                       serr[err_ix], ' samples).'))
      }
      st <- tpred[err_ix] + cuetab$RTIME[1] + cuetab$MTICKS[1] * 1e6
      ablks <- data.frame(RECN = cuetab$RECN[err_ix],
                          RTIME = floor(st),
                          MTICKS = (st %% 1) * 1e6,
                          NSAMPS = serr[err_ix],
                          STATUS = -1)
      cuetab <- rbind(cuetab[c(1:err_ix), ],
                    ablks,
                    cuetab[c((err_ix + 1):nrow(cuetab)), ])
    } # end of while loop to check timing
  } # end of "if there is more than one block"
  
  err_ix <- which(terr > err_thr_sec & serr > err_thr_samp)
  if (length(err_ix) > 0){
    message(paste0(length(err_ix),
                   ' data overruns detected with maximum size ',
                   -min(terr),
                   ' seconds (',
                   -min(serr),
                   ' samples).'))
  }
  # The columns of cuetab are:
  #   %     (SUFFIX: 'wav' (or maybe swv) -- this one is NOT in the matlab version)
  #          1. RECN:  File number
  # %        2. RTIME: Start time of block (UNIX seconds)
  # %        3. MTICKS: Microsecond offset to first sample in block
  # %        4. NSAMPS: Number of samples in the block
  # %        5. STATUS: Status of block (0=zero-filled, 1=data bearing, -1=data gap)
  
  # nominate a reference time and refer the cues to this time
  ref_time <- cuetab$RTIME[1] + cuetab$MTICKS[1] * 1e6 # time of 1st sample in deployment
  cuetab$ctimes <- (cuetab$RTIME - cuetab$RTIME[1]) + (cuetab$MTICKS - cuetab$MTICKS[1]) * 1e-6
  # rename cols of cuetab and keep desired columns
  names(cuetab) <- tolower(names(cuetab))
  names(cuetab)[names(cuetab) == "ctimes"] <- "start_sec"
  names(cuetab)[names(cuetab) == "nsamps"] <- "n_samples"
  cuetab <- cuetab[, c("recn", "start_sec", "n_samples", "status")]
  rownames(cuetab) <- NULL
  
  # compute file size in samples
  sz <- rep(0, nrow(sm_fnames))
  for (f in c(1:nrow(sm_fnames))){
    sz[f] <- sum(cuetab$n_samples[cuetab$recn == sm_fnames$recn[f] &
                                    cuetab$status  >= 0])
  }
  
  # change format of block status from 0==data to 1==data
  cuetab$status[cuetab$status >= 0] <- as.numeric(cuetab$status[cuetab$status >= 0] == 0)
  
  # collect all this information needed into the SA (sound archive) list
  # note: if there are more versions of SMRT tags or if this function is to be used
  # for other Dtypes, some of these quantities should be pulled from a lookup table.
  # and some related to gain and cal should perhaps come from the xml files or a device-specific lookup table.
  SA <- list()
  SA$depid <- depid
  SA$type <- "archive"
  SA$name <- "SA"
  SA$full_name <- "sound_archive"
  SA$description <- "sound data archive listing"
  SA$file_names <- paste(paste0(sm_fnames$file_name, ".wav"), collapse = ", ")
  SA$file_number <- nrow(sm_fnames)
  SA$file_format <- "wav"
  SA$file_resolution <- wav_info$audio$bitrate / wav_info$audio$sample_rate / wav_info$audio$channels
  SA$file_compression <- "none"
  SA$file_size <- sz
  SA$file_size_unit <- "samples"
  SA$archive_status <- "complete"
  SA$channel_num <- wav_info$audio$channels
  SA$channel_separation <- 0
  SA$channel_separation_unit <- "m"
  # SA$channel_sensitivity <- -172 # EEP is this right for smrt? should not be hardwired?
  SA$channel_sensitivity_unit <- "Decibels re volt per micropascal"
  SA$chennel_sensitivity_label <- "dB re V/muPa"
  SA$channel_sensitivity_explanation <- "Total recording sensitivity from water to wav file denoting full-scale in the wav file as 1.0 Volt"
  SA$channel_sensitivity_includes_gain <- "yes"
  SA$channel_gain <- 12
  SA$channel_gain_unit <- "Decibels"
  SA$sampling <- "regular"
  SA$sampling_rate <- xml_info$afs
  SA$sampling_rate_unit <- "Hz"
  SA$sampling_3dB_low <- "unknown"
  SA$sampling_3dB_high <- "unknown"
  SA$data <- data.matrix(cuetab)
  SA$data_row_name <- "block"
  SA$data_column_name <- paste(names(cuetab), collapse = ", ")
  SA$column_description_status <- "1 = data, 0 = zero-filled, -1 = gap"
  SA$column_description_recn <- "number of file in file_names"
  SA$column_description_n_samples <- "number of sound samples per channel in block"
  SA$contig_within_files <- "yes"
  SA$contig_across_files <- "yes"
  SA$start_time <- as.character(xml_info$recording_start)
  SA$start_time_tzone <- "UTC"
  SA$calibration_method <- "unknown"
  SA$calibration_date <- "unknown"
  SA$selfnoise_file <- paste0(sm_fnames$file_name[1], ".wav")
  SA$selfnoise_cue_start <- 0
  SA$selfnoise_cue_end <- 6
  SA$selfnoise_cue_unit <- "second into file"
  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
  SA$creation_date <- as.character(now)
  SA$history <- c("sound_archive")
  SA <- SA[sort(names(SA))]
  
  return(SA)
} # end of sound_archive

