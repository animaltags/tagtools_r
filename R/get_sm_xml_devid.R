#' Get SMRT tag device ID from xml file(s)
#'
#' Parse xml file to get information about sensor configuration and tag metadata from SM board of SMRT tag
#' @param xml_file filename (with path) of xml file to parse, or xml document as already read in using xml2::read_xml 

#' @return a list with entries device_id (the ID as a hexadecimal string) and device_serial (the short ID -- an 8-digit number) 
#' @export

get_sm_xml_devid <- function(xml_file) {
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Input checking
  if (missing(xml_file)){
    stop("get_sm_xml_devid() requires xml_file input")
  }

  # read in xml file (if not input as xml doc already)
  if (!("xml_document" %in% class(xml_file))){
    if (!grepl(xml_file, pattern = ".xml", fixed = TRUE)){
      xml_file <- paste0(xml_file, ".xml")
    }
    xml_file <- xml2::read_xml(xml_file)
  }

  xml_devid <- list()
  
  # device serial number / ID number
  xml_devid$device_id <- 
    xml2::xml_find_all(xml_file, "DEVID") |> 
    xml2::xml_text() |> 
    stringr::str_trim()
  
  # convert to device number
  id_vec <- stringr::str_split(xml_devid$device_id, pattern = " ", simplify = TRUE)
  # the number of pieces (and which ones to grab) is a DTAG thing I don't get
  # the prepending of "0x" tells r this is a hexadecimal base 0
  if (length(id_vec) < 4){
    id_vec <- paste0(id_vec[1:2], collapse = "")
  }else{
    id_vec <- paste0(id_vec[3:4], collapse = "")
  }
  
  # note: can't just use strtoi() because it only handles integers up to 2^31.
  xml_devid$device_serial <- as.numeric(paste0("0x", id_vec))

  return(xml_devid)
} # end of get_sm_xml_devid()

