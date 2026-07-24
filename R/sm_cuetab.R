#' Get "cue tab" (information about file timing) for SM board wav or swv data files
#'
#' this function is called by other functions that read SM board data files. It is not normally called by end users.
#' @param sm_dir directory where data files from the SM board or DTAG (e.g., xml and swv files) are stored
#' @param sm_file_info a data frame with information about SM data file names in sm_dir. If not input, it is obtained by a call to \code{\link{sm_fnames}}; providing it as input may save a little time.
#' @param xml_info a list of metadata extracted from the SM xml files. If not input, it is obtained by a call to \code{\link{sm_get_config}}; providing it as input may save a little time.
#' @param err_thr_sec threshold in seconds for reporting timing errors (that also exceed err_thr_samp). Default: 0.005 sec.
#' @param err_thr_samp threshold in samples for reporting timing errors (that also exceed err_thr_sec). Default: 10 samples.
#' @param suffix file extension of the files to catalog. They must be in a wav-format configuration. Default: 'wav' (could also be 'swv' or some other file extension assigned in the future to files in wav format).

#' @return a "cuetab" data.frame including variables:
#' 		\itemize{
#' 		\item {recn: recording number}
#' 		\item {start_sec: start time of block, in seconds since start of recording}
#' 		\item {n_samples: total number of samples in the block}
#' 		\item {status: whether data in this block is data (status = 1), zero-filled (status = 0), or and unfilled gap (status = -1)}
#' 		}
#' @export

sm_cuetab <- function(sm_dir,
                      sm_file_info = sm_fnames(sm_dir),
                      xml_info = sm_get_config(sm_dir),
                      err_thr_sec = 0.005,
                      err_thr_samp = 10,
                      suffix = 'wav') {
  # input check
  if (missing(sm_dir)){
    stop("sm_dir is required input to generate a cuetab")
  }
  
  # make sure required package for reading xml files is installed
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" must be installed to use this function to read sensor xml files.",
      call. = FALSE
    )
  }
  # make sure required package for reading wav files is installed
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function to read wav and swv files.",
      call. = FALSE
    )
  }
  
  sm_dir <- sm_dir_check(sm_dir)
  
  # get wav block info corresponding to each wav file
  check_xml_wavblk <- TRUE
  cuetab <- list() # first a list of data frames, to be rbind-ed later
  for (k in c(1:nrow(sm_file_info))){
    cuetab[[k]] <- data.frame() # cuetab is obtained/used separately for each file (not ultimately output)
    if (check_xml_wavblk){
      this_doc <- xml2::read_xml(paste0(sm_dir, sm_file_info$file_name[k], ".xml"))
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
    wavt_file <- paste0(sm_dir, sm_file_info$file_name[k], ".wavt")
    if (file.exists(wavt_file)){
      wavt_data <- utils::read.csv(wavt_file)
      cuetab[[k]] <- rbind(cuetab[[k]], wavt_data[wavt_data$SUFFIX == suffix,])
    }else{
      # apparently there is an "old" style of timing files that might have extension .swvt
      if (file.exists(paste0(sm_dir, sm_file_info$file_name[k], ".", suffix, "t"))){
        suffixt_data <- utils::read.csv(paste0(sm_dir, sm_file_info$file_name[k], ".", suffix, "t"))
        cuetab[[k]] <- rbind(cuetab[[k]], suffixt_data[suffixt_data$SUFFIX == suffix, ])
      }
    }
    
    # get sample rate & count from kth wav file & check consistency with xml / cuetab
    wav_file <- paste0(sm_dir, sm_file_info$file_name[k], '.', suffix)
    if (!file.exists(wav_file)){
      warning(paste0('No ', suffix, ' file found for recording ',  sm_file_info$file_name[k], ' - skipping...'))
    }else{
      # as long as the kth wav (or swv) file exists...
      # get acoustic recording metadata
      wav_info <- av::av_media_info(wav_file)
      # check that wav file and xml file sampling rates agree
      xml_fs <- ifelse(suffix == "wav", xml_info$afs, xml_info$fb)
      if (wav_info$audio$sample_rate != xml_fs){
        warning(paste0('Sampling rate mismatch in recording: ', sm_file_info$file_name[k]))
      }
      if (nrow(cuetab[[k]]) > 0){
        # check that total samples info from wav file and wavblks are same
        if (round(as.numeric(wav_info$duration * wav_info$audio$sample_rate)) !=
            as.numeric(sum(cuetab[[k]]$NSAMPS))){
          warning(paste0('Sample count mismatch in recording: ', sm_file_info$file_name[k]))
        }
      }else{
        # some DTAG3 xml files made with an old version of d3read don't have WAVBLK fields
        warning("No WAVBLK fields found in xml files - check version of d3read and re-run?")
        if (!check_xml_wavblk){
          this_doc <- xml2::read_xml(paste0(sm_dir, sm_file_info$file_name[k], ".xml"))
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
    if (nrow(cuetab[[k]]) > 0){
      cuetab[[k]]$RECN <- k      
    }
  }# end loop over wav/xml/wavt data files
  cuetab <- do.call(rbind, cuetab)
  
  if (is.null(xml_fs)){
    warning("Warning: Unable to determine sampling rate for this configuration")
  }
  
  # if cuetab has at least 2 rows, check the timing
  if (nrow(cuetab) > 1){
    frst <- 1
    overrun <- 0
    while (1){
      tpred <- cumsum(as.numeric(cuetab$NSAMPS[c(1:(nrow(cuetab) - 1))])) / xml_fs
      tnxt <- (cuetab$RTIME[c(2:nrow(cuetab))] - cuetab$RTIME[1]) + (cuetab$MTICKS[c(2:nrow(cuetab))] - cuetab$MTICKS[1])*1e-6
      terr <- tnxt - tpred
      serr <- round(terr * xml_fs)
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
        message(paste0('Gap in file ', sm_file_info$file_name[err_ix], 'of ', 
                       round(terr[err_ix], digits = 3), ' seconds (',
                       serr[err_ix], ' samples).'))
      }else{
        message(paste0('Gap between files ', sm_file_info$file_name[err_ix],
                       ' and ', sm_file_info$file_name[err_ix + 1], ' of ',
                       round(terr[err_ix], digits = 3), ' seconds (',
                       serr[err_ix], ' samples).'))
      }
      st <- tpred[err_ix] + cuetab$RTIME[1] + cuetab$MTICKS[1] * 1e-6
      ablks <- data.frame(RECN = cuetab$RECN[err_ix],
                          RTIME = floor(st),
                          MTICKS = (st %% 1) * 1e6,
                          NSAMPS = serr[err_ix],
                          STATUS = -1,
                          SUFFIX = suffix)
      cuetab <- rbind(cuetab[c(1:err_ix), ],
                      ablks,
                      cuetab[c((err_ix + 1):nrow(cuetab)), ])
    } # end of while loop to check timing
    err_ix <- which(terr > err_thr_sec & serr > err_thr_samp)
    if (length(err_ix) > 0){
      message(paste0(length(err_ix),
                     ' data overruns detected with maximum size ',
                     -min(terr),
                     ' seconds (',
                     -min(serr),
                     ' samples).'))
    }
  } # end of "if there is more than one block"
  

  # The columns of this initial cuetab are:
  #   %     (SUFFIX: 'wav' (or maybe swv) -- this one is NOT in the matlab version)
  #          1. RECN:  File number
  # %        2. RTIME: Start time of block (UNIX seconds)
  # %        3. MTICKS: Microsecond offset to first sample in block
  # %        4. NSAMPS: Number of samples in the block
  # %        5. STATUS: Status of block (1=zero-filled, 0=data bearing, -1=data gap) THIS GETS CHANGED BELOW TO 0 = zerofilled
  
  # nominate a reference time and refer the cues to this time
  ref_time <- cuetab$RTIME[1] + cuetab$MTICKS[1] * 1e6 # time of 1st sample in deployment
  cuetab$ctimes <- (cuetab$RTIME - cuetab$RTIME[1]) + (cuetab$MTICKS - cuetab$MTICKS[1]) * 1e-6
  # rename cols of cuetab and keep desired columns
  names(cuetab) <- tolower(names(cuetab))
  names(cuetab)[names(cuetab) == "ctimes"] <- "start_sec"
  names(cuetab)[names(cuetab) == "nsamps"] <- "n_samples"
  cuetab <- cuetab[, c("recn", "start_sec", "n_samples", "status")]
  rownames(cuetab) <- NULL
  
  # change format of block status from 0==data to 1==data
  cuetab$status[cuetab$status >= 0] <- as.numeric(cuetab$status[cuetab$status >= 0] == 0)
  
  return(cuetab)
}

