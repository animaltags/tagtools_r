#' Create a "sound archive" (SA) list for SMRT wav files
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
  sm_dir <- sm_dir_check(sm_dir)
  
  # get data.frame with file_name, sm_dir, recn, device_serial, device_id
  sm_file_info <- sm_fnames(sm_dir, depid)
  xml_file <- paste0(sm_dir, sm_file_info$file_name[1], ".xml")
  
  # get metadata from xml file
  xml_info <- sm_get_config(xml_file = xml_file)
  # note: xml_info$afs is acous sampling rate
  
  # get cuetab
  cuetab <- sm_cuetab(sm_dir, sm_file_info, xml_info, suffix = 'wav')
  
  if (nrow(cuetab) > 0){
  # compute file size in samples
  sz <- rep(0, nrow(sm_file_info))
  for (f in c(1:nrow(sm_file_info))){
    sz[f] <- sum(cuetab$n_samples[cuetab$recn == sm_file_info$recn[f] &
                                    cuetab$status  >= 0])
  }
  
  wav_file <- paste0(sm_dir, sm_file_info$file_name[1], '.', suffix)
  # get acoustic recording metadata
  wav_info <- av::av_media_info(wav_file)
  
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
  SA$file_names <- paste(paste0(sm_file_info$file_name, ".wav"), collapse = ", ")
  SA$file_number <- nrow(sm_file_info)
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
  SA$selfnoise_file <- paste0(sm_file_info$file_name[1], ".wav")
  SA$selfnoise_cue_start <- 0
  SA$selfnoise_cue_end <- 6
  SA$selfnoise_cue_unit <- "second into file"
  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
  SA$creation_date <- as.character(now)
  SA$history <- c("sound_archive")
  SA <- SA[sort(names(SA))]
  }else{SA <- list()} # (if there are not wav files return SA empty)
  
  return(SA)
} # end of sound_archive

