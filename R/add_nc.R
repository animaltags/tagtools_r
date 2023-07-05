#' Save an item to a NetCDF or add one tag sensor or metadata variable to a NetCDF archive file.
#'
#' Add one tag sensor or metadata variable to a NetCDF archive file. If the archive file does not exist,
#' it is created. The file is assumed to be in the current working directory
#' unless a pathname is added to the beginning of fname.
#'
#' @param file The name of the netCDF file to which to save. If the name does not include a .nc suffix, this will be added automatically.
#' @param D The sensor data or metadata list to be saved. 
#' @param vname The name of the sensor data stream to be saved. Defaults to the entry "name" from the sensor or metadata list provided by the user (but an option to specify a name is provided to facilitate calling this function from \code{save_nc}).
#' @seealso \code{\link{save_nc}}, \code{\link{load_nc}}
#' @export
#' @examples 
#' add_nc("beaked_whale", njerk(BW$A), "Jerk")
#' 


add_nc <- function(file, D, vname) {
  # input checking
  if (!is.list(D) | "animaltag" %in% class(D)) {
    stop("add_nc can only save individual sensor or metadata structures. Use save_nc to save an mutliple data streams or a whole animaltag object.")
  }

  if (missing(vname)) {
    vname <- D$full_name
    if (length(vname == 0)) {
      vname <- substitute(D)
    }
  }

  # append .nc suffix to file name if needed
  if (!grepl("\\.nc", file)) {
    file <- paste(file, ".nc", sep = "")
  }

  # test if D is a metadata structure or a sensor structure
  if (length(D$depid) == 0) {
    stop("Items to save to netCDF files must be tag sensor or metadata list objects.")
  }

  # check that the deployment ID of D matches the one in the file if the file
  # already exists
  prev_depid <- c()
  if (file.exists(file)) {
    nc_conn <- ncdf4::nc_open(file)
    prev_depid <- ncdf4::ncatt_get(nc_conn, 0)$depid
    if (length(prev_depid) != 0) {
      if (!identical(prev_depid, D$depid)) {
        e_msg <- paste("Chosen file name is already associated with deployment id: ",
          prev_depid, ". Choose a different file name.\n",
          sep = ""
        )
        stop(e_msg)
      }

      prev_vars <- names(nc_conn$var)
      if (vname != "info") {
        if (vname %in% prev_vars) {
          e_msg <- paste("Variable ", vname,
            " already exists in file. Choose a different name.\n",
            sep = ""
          )
          stop(e_msg)
        }
      }
    }
  } # end of "if file already exists" checks

  # now ready to save the data or metadata
  if ("data" %in% names(D)) { # D is a sensor structure
    if (length(D$data) == 0) {
      # if D is empty...
      ncv <- ncdf4::ncvar_def(
        name = vname,
        units = "", # D$meta_unit,
        dim = list(),
        missval = NULL
      )
    } else { # if there is some data
      if (is.null(dim(D$data))) {
        # if data is a vector make it a column matrix
        D$data <- matrix(D$data, nrow = length(D$data))
      }
      dims <- list()
      dimnames <- c("samples", "axes", "3rd dim", "4th dim")
      dimnames <- paste(vname, dimnames)
      for (d in 1:length(dim(D$data))) {
        dims[[d]] <- ncdf4::ncdim_def(
          name = dimnames[d],
          units = "",
          vals = c(1:dim(D$data)[d]),
          create_dimvar = FALSE
        )
      }
      ncv <- ncdf4::ncvar_def(
        name = vname,
        units = "",
        dim = dims,
        missval = NULL,
        longname = ""
      )
      if (!file.exists(file)) {
      # if the file doesn't exist create it
      nc_conn <- ncdf4::nc_create(file, ncv)
      } else {
      # if file already exists add variable to it
      nc_conn <- ncdf4::nc_open(file, write = TRUE)
      nc_conn <- ncdf4::ncvar_add(nc_conn, ncv)
      }
      # then write the data into the variable ncv
      ncdf4::ncvar_put(nc_conn, ncv,
        vals = as.vector(D$data)
      ) # ,
      # count = dim(D$data))
    } # end of writing sensor data

    # add metadata (from sensor data structure)
    i_meta <- which(names(D) != "data")
    for (m in i_meta) {
      ncdf4::ncatt_put(nc_conn,
        varid = vname,
        attname = names(D)[m],
        attval = D[[m]]
      )
    }

    if (length(i_meta) == 0) {
      w_msg <- paste(
        "No metadata in variable ",
        vname, ".\n"
      )
      warning(w_msg)
    }
    
    # add creation data to variable
    ncdf4::ncatt_put(nc_conn,
      vname,
      attname = "creation_date",
      attval = as.character(Sys.time())
    )
  } # end of "if it's a sensor data structure"

  # When D is a metadata "info" structure
  if (length(D$data) == 0) {
    if (!file.exists(file)) {
      stop("A netCDF file can not be created without at least one sensor data variable.")
    } else {
      # if file already exists add variable to it
      nc_conn <- ncdf4::nc_open(file, write = TRUE)
    }

    # add metadata (from info/metadata data structure)
    i_meta <- which(names(D) != "data")
    for (m in i_meta) {
      ncdf4::ncatt_put(nc_conn,
        varid = 0,
        attname = names(D)[m],
        attval = D[[m]],
        prec = "text"
      )
    }
    # add creation date (global attribute)
    ncdf4::ncatt_put(nc_conn,
                     varid = 0,
                     attname = "creation_date",
                     attval = as.character(Sys.time())
    )
  } # end of "if metadata info structure"
  
  # whatever else was written, always
  # make sure info (global attributes) includes time of creation and depid
  # add creation data to variable
  ncdf4::ncatt_put(nc_conn,
                   varid = 0,
                   attname = "creation_date",
                   attval = as.character(Sys.time())
  )
  ncdf4::ncatt_put(nc_conn,
                   varid = 0,
                   attname = "depid",
                   attval = D$depid
  )

  ncdf4::nc_close(nc_conn)
}
