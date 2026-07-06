#' Get SMRT SM board configuration info from xml file(s)
#'
#' Parse xml file(s) to get information about sensor configuration and tag metadata from SM board of SMRT tag
#' @param sm_dir directory where xml file(s) are stored
#' @param xml_file filename (with path) of xml file to parse. If provided, sm_dir will be ignored.

#' @return A list of metadata including elements:
#' 		\itemize{
#' 		\item {device_id: ID of the SMRT tag as a string (hexadecimal representation of device_serial)}
#' 		\item {device_serial: ID number of the SMRT tag}
#' 		\item {recording_start: datetime when tag recording began}
#' 		\item {dtype: "D4" for DTAG4-type SM board}
#' 		\item {fb: base (lowest) sampling rate of sensors, in Hz}
#'    \item {afs: acoustic sampling rate, in Hz}
#' 		\item {CFG: list of configuration information about sensor data stored in swv files. CFG$CHANS[[1]] is a string containing a comma-separated list of sensor channel ID numbers. The list xml_info$CFG$CHANS has attribute "N" specifying the number of channels.}
#' 		\item {sid: index for location of sensor info within xml file}
#' 		\item {n_chans} number of sensor channels recorded in swv files
#' 	  \item {all_channels} names of all channels in swv files
#' 	  \item {unique_channels} names of unique channels in swv files (some sensors record data in multiple channels, if their sampling rate is higher than fb)
#'    \item {sampling_rate} sampling rate (in Hz) of each channel in unique_channels. These values match fb * (# of times the unique channel appears in all_channels)
#' 		}
#' @export

sm_get_config <- function(sm_dir = NULL,
                          xml_file = NULL) {
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Input checking
  if (is.null(sm_dir) & is.null(xml_file)){
    stop("sm_get_config() requires either sm_dir or xml_file input")
  }
  
  if (is.null(xml_file)){
    # check formatting of sm_dir input
    sm_dir <- sm_dir_check(sm_dir)
    
    # try to get from xml file: device_serial
    if (is.null(xml_file) & !is.null(sm_dir) & dir.exists(sm_dir)){
      sm_files <- list.files(sm_dir)
      sm_xml <- sm_files[grepl(sm_files, pattern = ".xml")]
      xml_file <- paste0(sm_dir, sm_xml[1])
    }
  }
  
  # make sure xml_file has ".xml" at the end
  if (!grepl(xml_file, pattern = ".xml", fixed = TRUE)){
    xml_file <- paste0(xml_file, ".xml")
  }
  
  xml_doc <- xml2::read_xml(xml_file)

  xml_info <- sm_xml_devid(xml_doc)
  
  
  xml_info$recording_start <- 
    lubridate::ymd_hms(
      xml2::xml_find_first(xml_doc, "EVENT/@TIME") |> xml2::xml_text(),
      tz = "UTC")
  
  # Dtag "generation"
  xml_info$dtype <- xml2::xml_find_all(xml_doc, "DGEN") |> 
    xml2::xml_text() |> 
    stringr::str_trim()
    
  # sensor configuration info
  if (xml_info$dtype == "D3"){
    warning("Sensor configuration metadata extraction not implemented yet...")
    # need to implement as in getsensorcfg.m sub-fun check_d3cfg
  }
  
  # all SMRT tags should be D4 type
  xml_info$CFG <- NULL
  xml_info$fb <- 0
  if (xml_info$dtype == "D4"){
    xml_config <- xml2::xml_find_all(xml_doc, "CFG")
    for (k in c(1:length(xml_config))){
      this_config = xml_config[[k]]
      if ("PROC" %in% xml2::xml_name(xml2::xml_children(this_config))){
        if (grepl(pattern = "SENS", xml2::as_list(xml2::xml_find_all(this_config, "PROC")) |> unlist()) ||
            grepl(pattern = "ACC", xml2::as_list(xml2::xml_find_all(this_config, "PROC")) |> unlist())){
          xml_info$CFG <- xml2::as_list(this_config)
          xml_info$sid <- xml2::xml_attr(this_config, "ID")
          xml_info$fb <- xml2::xml_find_all(this_config, "FS") |> xml2::xml_double() # find the sensor sampling rate
          break
        } # end of if "SENS" or "ACC"
      } # end of if "PROC" 
    } # end of loop over entries of xml_config (k) 
    for (k in c(1:length(xml_config))){
      this_config = xml_config[[k]]
      # get information about acoustic recordings
      # look for items with entry FTYPE with value "wav" AND entry SUFFIX matching input suffix
      if (!is.na(xml2::xml_find_first(this_config, "@FTYPE")) &&
          !is.na(xml2::xml_find_first(this_config, "SUFFIX"))){
        if (grepl(pattern = "wav", xml2::xml_find_first(this_config, "SUFFIX") |> xml2::xml_text())){
          # pull out values of FS and EXP: acous fs is FS * 10^EXP (if not there EXP is 0)
          afs0 <- as.numeric(xml2::xml_find_all(this_config, "FS") |> xml2::xml_double())
          if ("EXP" %in% xml2::xml_name(xml2::xml_children(this_config))){
            expn <- xml2::xml_find_first(this_config, "EXP") |> xml2::xml_double()
          }else{
            expn <- 0
          }
          xml_info$afs0 <- afs0
          xml_info$afs <- afs0 * 10^expn
          break
          # note the original dtag function deals with cases of duty cycling which we have NOT
        } # end of extracting FS and EXP
      } # end of if "FTYPE" and "SUFFIX" are present
    } # end of loop over entries of xml_config (k) 
  } # end of if D4
  
  if (is.null(xml_info$CFG)){
    return(xml_info)
  }
  
  # check for a decimator - need to do this because the channel assignments change

  for (k in c(1:length(xml_config))){
    this_config = xml_config[[k]]
    if ("PROC" %in% xml2::xml_name(xml2::xml_children(this_config))){
      if (grepl(pattern = "SDEC", xml2::as_list(xml2::xml_find_all(this_config, "PROC")) |> unlist())){
        if (xml2::xml_find_all(this_config, "SRC/@ID") |> xml2::xml_text() == xml_info$sid){
          xml_info$CFG$CHANS[[1]] <- xml2::xml_find_all(this_config, "CHANS") |> xml2::xml_text() |> unlist()
          attributes(xml_info$CFG$CHANS)$N <- xml2::xml_find_all(this_config, "CHANS/@N") |> xml2::xml_double()
          break
        } # if SRC node attribute ID matches sid
      } # if SDEC
    } # end of if "PROC"
  } # end of loop over k entries of xml_config
  
  # number of channels
  xml_info$n_chans <- attr(xml_info$CFG$CHANS, "N")
  
  # channel ID numbers
  chans <- 
    as.numeric(
      unlist(
        stringr::str_split(xml_info$CFG$CHANS[[1]], pattern = ", ")))
  
  xml_info$all_channels <- chans
  xml_info$unique_channels <- unique(chans)
  
  # group channels
  xml_info$sampling_rate <- rep(0, length(xml_info$unique_channels)) 
  for (k in c(1:length(xml_info$unique_channels))){
    kk = which(chans == xml_info$unique_channels[k])
    xml_info$sampling_rate[k] = xml_info$fb * length(kk)
  }
  
  if (xml_info$n_chans != length(xml_info$all_channels)){
    warning("n_chans does not match the number of channels listed in all_channels; check SM board .xml file.")
  }
  
  # keep the whole document also to avoid need to read it multiple times
  xml_info$xml_doc <- xml_doc

  return(xml_info)
} # end of sm_get_config()

