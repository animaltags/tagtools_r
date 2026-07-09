#' Check sm_dir input for formatting
#'
#' this function is called by other functions that read SM board data files. It checks the format of the SM path provided by the user. It is not normally called by end users.
#' @param sm_dir directory where data files from the SM board or DTAG (e.g., xml and swv files) are stored

#' @return sm_dir formatted as tagtools functions expect (including final / and using / not \)
#' @export

sm_dir_check <- function(sm_dir) {
  # Input checking
  if (missing(sm_dir)){
    stop("sm_dir is a required input")
  }
  
  # make sure sm_dir ends with / (and uses only / not \, for mac compatibility)
  if (!missing(sm_dir)){
    if (!stringr::str_ends(sm_dir, pattern = stringr::fixed("/"))){
      sm_dir <- paste0(sm_dir, "/")
    }
    sm_dir <- gsub(sm_dir, pattern = "\\", replacement = "/", fixed = TRUE)
  }
  
  if (!missing(sm_dir) & !dir.exists(sm_dir)){
    stop(paste("Folder ", sm_dir, " not found. Please check sm_dir input." ))
  }
  return(sm_dir)
}

