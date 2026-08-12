#' Get SMRT SM board sensor calibration info from xml file(s)
#'
#' Parse xml file(s) to get information about sensor calibration constants from SM board of SMRT tag
#' @param sm_dir directory where xml file(s) are stored
#' @param xml_file filename (with path) of xml file to parse. If provided, sm_dir will be ignored.
#' @param attr_path directory containing device-specific attribute files. Most SMRT devices do not have these, so if you don't know what it is or don't have any, leave \code{attr_path} at its default NULL value. In this case,  generic (factory preset values based on sensor specifications) will be used, which (in combination with data-driven calibrations) is sufficient in most cases.

#' @note DTAG and Matlab compatibility: This function replicates some of the tasks of DTAG Matlab tools d4readattr and d4decodeattr.
#' @return A "CAL" list of calibration constants and metadata including elements:
#' 		\itemize{
#' 		\item {PRESS}
#' 		\item {TEMP}
#' 		\item {ACC}
#' 		\item {MAG}
#' 		\item {AUDIO}
#' 		}
#' @export

sm_get_cal <- function(sm_dir = NULL,
                       xml_file = NULL,
                       attr_path = NULL) {
  
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package \"xml2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  # Input checking
  if (is.null(sm_dir) & is.null(xml_file)){
    stop("sm_get_cal() requires either sm_dir, xml_file, or xml_info input")
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
  xml_info <- sm_get_config(xml_file = xml_file)
  
  # get accel full-scale setting
  accsel <- xml_info$CFG$ACC_FSR |> as.numeric()
  # options are 2, 4, 8. In some early SMRT records it's 1 and that's an error (should be 8)
  if (length(accsel == 0)){
    accsel <- 8 # default accelerometer setting is 8g
  }
  if (accsel == 1){
    accsel <- 8
    warning("Unexpected accelerometer full-scale value of 1 found in SMRT xml. Correcting to 8. This change (bug fix) is expected for some early SMRT deployments but check data files if that is not the case for you.")
  }
  
  # get mag full-scale setting
  magsel <- xml_info$CFG$MAG_FSR |> as.numeric()
  if (length(magsel) == 0){
    magsel <- 4 # default
  }
  
  # get audio gain
  xml_gain <- xml2::xml_find_all(xml_doc, "EVENT") |> xml2::xml_find_first("AUDIO") |> xml2::xml_attr("GAIN")
  audsel <- as.numeric(xml_gain)[!is.na(xml_gain)][1]
  if (length(audsel) == 0){
    audsel <- 2 # default: high gain
  }
  
  # note: SMRT tag xml files don't include anything about pressure sensors (WC board does pressure)
  # there are some possible cal constants in the CFG for FTYPE = "csv" but not sure what to use them on/how so
  psel <- 0
  
  sensors <- c("PRESS", "TEMP", "ACC", "MAG", "AUDIO")
  CAL <- lapply(sensors, function(x) get_CAL0(x))
  names(CAL) <- sensors
  
  # for DTAG3/4 we would now look in the xml files for attribute (CAL) data. SMRT tags do not have this.
  # next we look for a device-specific attribute file. it is unlikely most SMRT devices will have one.
  if (!is.null(attr_path)){
    attr_devid <- paste0(unlist(strsplit(xml_info$device_id, split = " ")), collapse = "_")
    # this will either be a file name with path or character NA
    my_attr_file <- list.files(path = attr_path,
                               pattern = attr_devid,
                               fixed = TRUE,
                               full.names = TRUE)[1]
  }else{
    my_attr_file <- as.character(NA)
  }
  
  if (!file.exists(my_attr_file)){
    # else (in most cases) we use a default SMRT attribute file. 
    # This contains factory cal constants (not from a device-specific bench cal) 
    # but in combo with autocal tools generally works fine across SMRT devices.
    my_attr_file <- system.file("extdata/attr_smrt_generic.txt", package = "tagtools")
  }
  smrt_attr <- data.frame(t(utils::read.csv(file = my_attr_file, skip = 4, header = FALSE)))
  names(smrt_attr) <- smrt_attr[1,]
  smrt_attr <- smrt_attr[2:nrow(smrt_attr), ]
  
  if ("ACC" %in% names(CAL) & paste0("A", accsel) %in% names(smrt_attr)){
    attr_vals <- utils::tail(as.numeric(smrt_attr[, paste0("A", accsel)]), -1)
    n <- utils::head(as.numeric(smrt_attr[, paste0("A", accsel)]), 1)
    r <- 9.81 * t(matrix(attr_vals/1000, nrow = 2, ncol = 3))
    CAL$ACC$range <- accsel * 10
    CAL$ACC$poly <- r
    CAL$ACC$map <- diag(1, 3, 3)
    CAL$ACC$tref <- 20
    CAL$ACC$tcomp <- matrix(0, nrow = 3, ncol = 1)
    CAL$ACC$tcompsrc <- "temp"
    CAL$ACC$attr <- paste0("A", accsel)
    if ("AM" %in% names(smrt_attr)){
      CAL$ACC$map <- decodemap(as.numeric(smrt_attr[2, "AM"]))
    }
  }
  
  if ("MAG" %in% names(CAL) & "MH" %in% names(smrt_attr)){
    attr_vals <- utils::tail(as.numeric(smrt_attr[, "MH"]), -1)
    n <- utils::head(as.numeric(smrt_attr[, "MH"]), 1)
    r <- t(matrix(attr_vals/10, nrow = 2, ncol = 3))
    CAL$ACC$range <- 400
    CAL$ACC$poly <- r
    CAL$ACC$map <- diag(1, 3, 3)
    CAL$ACC$tref <- 20
    CAL$ACC$tcomp <- matrix(0, nrow = 3, ncol = 1)
    CAL$ACC$tcompsrc <- "temp"
    CAL$ACC$attr <- "MH"
    if ("MM" %in% names(smrt_attr)){
      CAL$MAG$map <- decodemap(as.numeric(smrt_attr[2, "MM"]))
    }
  }
  
  if ("AUDIO" %in% names(CAL)){
    if (audsel == 1){
      attr_vals <- utils::tail(as.numeric(smrt_attr[, "SL"]), -1)
      CAL$AUDIO$attr <- "SL"
    }else{
      attr_vals <- utils::tail(as.numeric(smrt_attr[, "SH"]), -1)
      CAL$AUDIO$attr <- "SH"
    }
    CAL$AUDIO$sens <- attr_vals[1]
    CAL$AUDIO$sens_unit = "dB re U/uPa"
    CAL$AUDIO$bandwidth = sort(attr_vals[2:3] * 10)
  }
  
# Note: CAL$PRESS and CAL$TEMP are not filled in as these sensors are not on the SM board
  
  return(CAL)
} # end of sm_get_cal()

get_CAL0 <- function(sensor){
  # this sensor structure isn't really used/saved, it is just to grab info from system file sensor_names.csv
  X <- suppressWarnings(sens_struct(data = NULL, type = sensor, depid = "dummy"))
  CAL0 <- list(unit = X$unit, 
               type = X$type,
               unit_name = X$unit_name,
               unit_label = X$unit_label)
  if ('axes' %in% names(X)){
    CAL0$axes <- X$axes
  }
  return(CAL0)
}

decodemap <- function(p){
  MKEY <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0, -1, -1, 0, 0),
                 nrow = 3,
                 ncol = 8,
                 byrow = FALSE)
  xmap <- floor(p / 256)
  ymap <- floor((p %% 256) / 16 )
  zmap <- (p %% 16)
  M <- cbind(MKEY[,xmap + 1], MKEY[, ymap + 1], MKEY[,zmap + 1])
  return(M)
}
