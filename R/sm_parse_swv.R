#' Read in one swv file from a SMRT tag's SM board, and integrate information about file timing and gaps
#'
#' SM boards in SMRT tags store metadata about file timing, as well as storing data from sensors (sampled perhaps at different rates) in swv or "sensor wav" files. This function reads in one such .swv file. This function is not normally called by users, but instead called by read_smrt_sm(), which calls sm_read_swv(), which calls this function. (The file names and function call structure loosely mimic the DTAG Matlab tool kit of Mark Johnson, after which these tools are modeled.)
#' @param swv_file filename (with path) of swv file to be read
#' @param ch a vector of sensor numbers or names to read in. If ch is specified, only the sensor channels matching the type (or numbers) given will be read. To find out which channels are available in a dataset, use: \code{\link{sm_channels}}.
#' @param start_samp time (in samples since file start) to start reading data. Default: start of file.
#' @param end_samp time (in samples since file start) to start reading data. Default: end of file.
#' @param depid Deployment ID string. Optional, but may be helpful to include if the directory where .swv files are stored contains data from more than one tag deployment.

#' @return A list containing metadata and sensor data vectors (for triaxial sensors, there will be one per axis). Each may be a different length according to the sampling rate of the sensor channel.
#' 		\itemize{
#' 		\item {data: sensor data vectors (each vector's length depends on sampling rate; number of entries in data matches length of sampling_rate and ch_nums)}
#' 		\item {sampling_rate: vector of sampling rates. Length corresponds to the number of sensors represented in \code{data}.}
#' 		\item {ch_nums: vector of channel ID numbers. Length corresponds to the number of sensors represented in \code{data}.}
#'    \item {ch_names: vector of detailed names of channels}
#' 		}
#' @export

sm_parse_swv <- function(swv_file,
                        ch = NULL,
                        start_samp = NULL,
                        end_samp = NULL,
                        depid = NULL){
  
  # make sure required package for reading wav files is installed
  if (!requireNamespace("av", quietly = TRUE)) {
    stop(
      "Package \"av\" must be installed to use this function to read sensor wav (swv) files.",
      call. = FALSE
    )
  }
  
  # remove file extension from input file name (if given)
  swv_file <- tools::file_path_sans_ext(swv_file)
  
  # get path to directory where sm (.swv, .xml) files are stored
  sm_dir <- dirname(swv_file)
  sm_dir <- check_sm_dir(sm_dir)
  
  # get sm board configuration metadata from xml files in sm_dir
  # this includes sensor channel count (xml_info$n_chans) 
  # and ID numbers (xml_info$all_channels and xml_info$unique_channels)
  # and xml_info$sampling_rate (sensor sampling rate, length matches unique_channels)
  # xml_info$sampling_rate also matches .swv file sampling rate * (# of times the unique channel is in all_channels)
  xml_info <- get_sm_config(sm_dir)
  
  # get metadata about channels (names, numbers, etc.)
  # if user has input a subset of channels to read in input ch, this will be subsetted accordingly
  # (else all channels in the data files will be included)
  # future note: this function does not require depid input. without it things may not work well if swvs from MANY deployments are stored in the same directory.
  sensor_defs <- sm_channels(ch, sm_dir, depid)
  
  # notes on packages to read wav files:
  # package wav puts ALL channels into one humongous single vector. There is no way to obtain metadata w/o reading the whole file, nor to read part of a file.
  # package tuneR has an annoying (to us?) WavMC class for the data. Channel extraction tools are designed only for simple stereo audio, really - can use [,] indexing but the output object is another one of class WavMC. Package does offer functions to get metadata and to normalize data.
  # package audio can load data to a matrix but NO partial reading and no grabbing metadata without the data and NO control over the scaling
  # package av also puts all channels into one vector w/o builtin way to access certain channels. It has a function to read metadata. It allows subset by seconds only (no option for samples so some precision could be lost)
  # speed: wav > av >> tuneR
  # pkg     mean  median
  # <fct>  <dbl>   <dbl>
  #   1 tuneR 0.215  0.158  
  # 2 av    0.0443 0.0339 
  # 3 wav   0.0272 0.00785
  # here we choose av for combination of speed and functionality (ability to grab metadata and read part of file if needed).
  # NOTE if changing to another function the scheme used below to normalize will need re-verification,
  # since we probably won't know for sure what format/encoding the data are read in as.
  
  wav_info <- av::av_media_info(paste0(swv_file, ".swv"))
  
  if (!is.null(end_samp)){
    # if given, end_sec can't be more than the dur of the file
    end_sec <- min(wav_info$duration, end_samp * wav_info$audio$sample_rate)    
  }
  
  if (!is.null(start_samp)){
    start_sec <- start_samp * wav_info$audio$sample_rate
  }else{
    start_sec <- NULL
    }
  
  swv_base <- av::read_audio_bin(paste0(swv_file, ".swv"),
                     start_time = start_sec,
                     end_time = end_sec)
  # normalize from binary PCM to interval (-1,1)
  # bit depth of recording
  # the 2* is because of the signs, I think? Else doesn't work and doesn't match matlab
  bitz <- 2 * wav_info$audio$bitrate / wav_info$audio$sample_rate / wav_info$audio$channels
  # max PCM value
  max_int <- switch(as.character(bitz),
              "1" = 1,
              "8" = 128,
              "16" = 32768,
              "24" = 8388608,
              "32" = 2147483648) 
  swv_base <- swv_base / max_int
  # end of normalization
  
  # reshape data into a matrix with 1 col per channel
  # this assumes standard channel interleaving like
  # c1samp1 c2samp1 c3samp1 c1samp2 c2samp2...
  # we could skip this step for speed except it's needed for checking for missing values
  swv_base <- matrix(swv_base, ncol = wav_info$audio$channels, byrow = TRUE)
  
  if (grepl(xml_info$dtype, "d3", ignore.case = TRUE)){
    # not sure if the code below (mimicking d3parseswv.m) is correct b/c
    # not sure in what format av::read_audio_bin() reads in the data.
    warning("DTAG3 data encoding in .swv not verified - check data carefully!")
    # if DTAG3 type SM board:
    # "convert from two's complement to offset binary" per DTAG tools
    swv_base[swv_base < 0] <- 2 + swv_base[swv_base < 0]
    # expected sensor reading range is 0 to 1, set this min/max
    swv_base <- swv_base / 2
    # replace fill values with missing values
    swv_base[swv_base == 0] <- NA
  }else{
    # if DTAG4 type SM board (should include all SMRT tags):
    # all columns that are entirely -1s should be NAs
    # (that sample was filled-in and should be treated as missing)
    swv_base[apply(swv_base, MARGIN = 1, FUN = function(x) all(x == -1)), ] <- NA
  }
  
  swv_data <- list()
  swv_data$ch_nums <- xml_info$unique_channels
  swv_data$sampling_rate <- xml_info$sampling_rate
  swv_data$data <- list()
  out_names <- vector(length = length(xml_info$unique_channels))
  for (k in c(1:length(xml_info$unique_channels))){
    # for each unique channel, gather the channels in all_channels of that name
    # then paste them together (if 3 for ex, ch1 s1 is c1 s1, order goes c1s1, c2s1, c2s1, c1s2, ...)
    swv_data$data[[k]] <- as.vector(t(swv_base[ , xml_info$all_channels == xml_info$unique_channels[k]]))
    out_names[k] <- sensor_defs$ch_names[sensor_defs$ch_nums == xml_info$unique_channels[k]]
  }
  swv_data$ch_names <- out_names
  names(swv_data$data) <- out_names
  
  # note: the d3builtincal() for DTAG3 tags IS NOT IMPLEMENTED here.
  
  return(swv_data)
} # end of sm_parse_swv

