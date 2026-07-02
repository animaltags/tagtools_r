#' Get SMRT SM data file names, given a directory to search and a deployment ID
#'
#' Get SMRT SM data file names, given a directory to search and a deployment ID
#' @param sm_dir directory where xml file(s) are stored
#' @param depid deployment ID string

#' @note Unlike the Matlab dtag tools, this function will not store results in the temporary/working directory. Info is re-read from sm_dir each time the function is called.
#' @return A data.frame of information about data files in sm_dir including variables:
#' 		\itemize{
#' 		\item {device_serial: ID number of the tag}
#' 		\item {file_name: base data file names (without file extensions like ".xml")}
#' 		\item {recn: recording number}
#' 		\item {sm_dir: file location (as input)}
#' 		}
#' @export

get_sm_fnames <- function(sm_dir,
                          depid = "") {
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Input checking
  sm_dir <- check_sm_dir(sm_dir)
  # (End of input checks)
  
  # get list of xml files in sm_dir including depid in the name
  sm_files <- list.files(sm_dir)
  
  if (length(sm_files) == 0){
    stop(paste0("No files found in folder ", sm_dir))
  }
  
  sm_xml <- sm_files[grepl(sm_files, pattern = ".xml") &
                       grepl(sm_files, pattern = depid)]

  if (length(sm_xml) > 0){
    # output data frame has file name stubs w/o extension (eg ".xml")
    sm_fnames <- data.frame(file_name = gsub(sm_xml, pattern = ".xml", replacement = "", fixed = TRUE),
                            sm_dir = sm_dir,
                            recn = 0,
                            device_serial = 0,
                            device_id = ''
                            )
    for (f in c(1:length(sm_xml))){
      xml_file <- xml2::read_xml(paste0(sm_dir, sm_xml[f]))
      xml_info <- get_sm_xml_devid(xml_file)
      sm_fnames[f, "device_serial"] <- xml_info$device_serial
      sm_fnames[f, "device_id"] <- xml_info$device_id
      # the record number is the last three characters of the extension-less file name
      sm_fnames[f, "recn"] <- as.numeric(stringr::str_sub(sm_fnames[f, "file_name"], start = -3, end = -1))
    }
  }else{
    stop(paste0("No .xml files with filenames including ", depid, " found in folder ", sm_dir))
  }

  return(sm_fnames)
} # end of get_sm_fnames()

