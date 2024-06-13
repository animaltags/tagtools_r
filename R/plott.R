#' Plot tag data time series
#'
#' Plot time series in a single or multi-paneled figure, using ggplot2 graphics for static graphs and plotly for interactive ones. This is useful, for example, for comparing measurements across different sensors in an animaltag data object. The time axis is automatically displayed in seconds, minutes, hours, or days according to the span of the data.
#'
#' If the input data X is an \code{animaltag} object, then all sensor variables in the object will be plotted. To plot only selected sensors from the \code{animaltag} object \code{my_tag}, for example, the input X=list(my_tag$A, my_tag$M) would plot just the accelerometer and magnetometer data. If possible, the plot will have
#'
#' @param X List whose elements are either lists (containing data and metadata) or vectors/matrices of time series data. See details.
#' @param fsx (Optional) A numeric vector whose length matches the number of sensor data streams (list elements) in X. (If shorter, \code{fsx} will be recycled to the appropriate length). \code{fsx} gives the sampling rate in Hz for each data object. Sampling rates are not needed when the data object(s) \code{X} are list(s) that contain sampling rate information -- and beware, because \code{fsx} (if given) will override sensor metadata.
#' @param r (Optional) Logical. Should the direction of the y-axis be flipped? Default is FALSE. If \code{r} is of length one (or shorter than the number of sensor data streams in X) it will be recycled to match the number of sensor data streams. Reversed y-axes are useful, for example, for plotting dive profiles which match the physical situation (with greater depths lower in the display). If the name of a sensor list is "P" or contains the word "depth", it will automatically be reversed.
#' @param offset (Optional) A vector of offsets, in seconds, between the start of each sensor data stream and the start of the first one. For example, if acceleration data collection started and then depth data collection commenced 436 seconds later, then the \code{offset} for the depth data would be 436.
#' @param date_time_axis (Optional) Logical. Should the x-axis units be date-times rather than time-since-start-of-recording?  Ignored if \code{recording_start} is not provided and \code{X} does not contain metadata on recording start time. Default is FALSE.
#' @param recording_start (Optional) The start time of the tag recording as a \code{\link{POSIXct}} object. If provided, the time axis will show calendar date/times; if not, it will show days/hours/minutes/seconds (as appropriate) since time 0 = the start of recording. If a character string is provided it will be coerced to POSIXct with \code{\link{as.POSIXct}}.
#' @param panel_heights (Optional) A vector of relative or absolute heights for the different panels (one entry for each sensor data stream in \code{X}). Default is equal-height panels. If \code{panel_heights} is a numeric vector, it is interpreted as relative panel heights. 
#' @param panel_labels (Optional) A list of y-axis labels for the panels. Defaults to names(X).
#' @param line_colors (Optional) A list of colors for lines for multivariate data streams (for example, if a panel plots tri-axial acceleration, it will have three lines -- their line colors will be the first three in this list). May be specified in any specification R understands for colors. Defaults to c("#000000", "#009E73", "#9ad0f3", "#0072B2", "#e79f00", "#D55E00")
#' @param interactive (Optional) Should an interactive figure (allowing zoom/pan/etc.) be produced? Default is TRUE. Interactive plotting requires the package plotly.
#' @param draw (Optional) Whether or not to draw the plot. Defaults to TRUE. If FALSE, a list of ggplot objects (if interactive is FALSE; this list is suitable to plot with \code{cowplot::plot_grid()}) or plotly object (if interactive is TRUE) will be returned.
#' @return A plot of time-series data created with ggplot or plotly. If you prefer base R graphics, consider function \code{\link{plott_base}} instead.
#' @export
#' @note This is a flexible plotting tool which can be used to display and explore sensor data with different sampling rates on a uniform time grid.
#' @examples
#' plott(list(depth = harbor_seal$P, Accel = harbor_seal$A))
#' 
plott <- function(X, fsx = NULL, r = FALSE, offset = 0,
                  date_time_axis = FALSE,
                  recording_start = NULL,
                  panel_heights = rep.int(1, length(X)) / length(X),
                  panel_labels = names(X), line_colors,
                  interactive = FALSE,
                  draw = TRUE) {
  
  if ("animaltag" %in% class(X)) {
    info <- X$info
    X <- X[!(names(X) %in% c("info", "_empty"))]
    if (length(panel_labels) > length(X)){
      panel_labels <- names(X)
    }
    if (length(panel_heights) > length(X)){
      panel_heights = names(X)
    }
    # consider: should irregularly sampled data streams be plotted at all? Probably as points not lines?
  }
  
  if (length(r) == 1) {
    if (r == FALSE) {
      r <- rep.int(r, length(X))
      zi <- grepl(pattern = "depth", x = names(X), ignore.case = TRUE) | (tolower(names(X)) == "p")
      r[zi] <- TRUE
    }else{
      r = rep.int(r, length(X)) # for unexpected case where r = TRUE is input and there are several sensors
    }
  }
  
  if (missing(line_colors)) {
    line_colors <- c("#000000", "#009E73", "#9ad0f3", "#0072B2", "#e79f00", "#D55E00")
  }
  if (length(offset) < length(X)) {
    offset <- rep(offset, length.out = length(X))
  }
  
  times <- list()
  fs <- numeric(length = length(X))
  for (s in 1:length(X)) {
    if (suppressWarnings(!missing(fsx) &
                         !sum(is.null(fsx)) &
                         !sum(is.na(fsx)))) {
      if (length(fsx) < length(X)) {
        fsx <- rep(fsx, length.out = length(X))
      } # end of recycling fsx to length(X)
      fs[s] <- fsx[s]
      if ("data" %in% names(X[[s]])) {
        # if X[[s]] is a sensor data structure
        n_obs <- min(nrow(X[[s]]$data), length(X[[s]]$data))
      } else {
        n_obs <- min(nrow(X[[s]]), length(X[[s]]))
      }
      times[[s]] <- c(-1 + (1:n_obs)) / fs[s] + offset[s]
    } else { # end of "if fsx is given"
      if (length(X[[s]]$sampling_rate) < 1) {
        if (grepl(pattern = 'irregular', X[[s]]$sampling, ignore.case = TRUE)){
          # for irregularly sampled sensors...there isn't a sampling rate available
          fs[s] <- NA
        }else{
          stop("If X does not contain sensor data lists (with sampling_rate entry), then fsx must be provided.")
        } 
      } else {
        fs[s] <- X[[s]]$sampling_rate
      }
      n_obs <- min(nrow(X[[s]]$data), length(X[[s]]$data))
      if (grepl(pattern = 'irregular', X[[s]]$sampling, ignore.case = TRUE)){
        time_col <- which(grepl('time', stringr::str_split(X[[s]]$column_name, pattern = ',')))
        if (length(time_col > 0)){
          time_col <- time_col[1]
        }else{
          time_col <- 1
        }
        # times are in seconds since start to begin with
        times[[s]] <- X[[s]]$data[,time_col] + offset[s]
      }else{ 
        times[[s]] <- c(-1 + (1:n_obs)) / fs[s] + offset[s]
      }
    }
  } # end loop over sensor streams to get times vectors
  x_lim <- range(sapply(times, range, na.rm = TRUE), na.rm = TRUE)
  
  # if recording_start is given or available,
  # then use date/time objects
  # ==============================================================
  if (date_time_axis) {
    if (exists("info")) {
      recording_start <- info$dephist_device_datetime_start
    }
    
    if (inherits(recording_start, "character")) {
      # try to coerce recording start time to POSIX if needed
      recording_start <- lubridate::ymd_hms(recording_start, tz = "GMT")
    }
    if (sum(grepl("POSIX", class(recording_start))) > 0) {
      times <- lapply(times, function(x, rs) {
        lubridate::seconds(x) +
          rs
      }, rs = recording_start)
      x_lim <- recording_start + lubridate::seconds(x_lim)
    } else {
      # not enough info for date_time_axis
      date_time_axis <- FALSE
    }
  }
  
  # adjust time axis units and get x axis label
  # ======================================================
  brk <- data.frame(secs = c(0, 2e3, 2e4, 5e5)) # break points for plots in seconds, mins, hours, days
  brk$units <- c("Time (sec.)", "Time (min.)", "Time (hours)", "Time (days)")
  brk$div <- c(1, 60, 3600, 24 * 3600) # divide time in sec by div to get new units
  
  if (sum(grepl("POSIX", class(times[[1]])))) {
    x_lab <- "Time"
  } else {
    t_ix <- min(length(brk$secs), match(1, max(x_lim) < brk$secs), na.rm = TRUE)
    for (i in 1:length(X)) {
      times[[i]] <- times[[i]] / as.numeric(brk[t_ix, "div"])
    }
    x_lim <- x_lim / as.numeric(brk[t_ix, "div"])
    x_lab <- as.character(brk[t_ix, "units"])
  }
  
  ### CHECK SIZE?
  # don't want to crash r, give warning if data is really big? TODO
  # set up plot structure / info
  # ===============================================================
  axis_names <- c('X', 'Y', 'Z')
  
  # draw INTERACTIVE plot
  # ===============================================================
  if (interactive){
    facet_list <- vector(mode = "list", length = length(X))
    for (i in 1:length(X)) {
      facet_list[[i]] <- plotly::plot_ly() |>
        plotly::layout(yaxis = list(title = panel_labels[i],
                                    autorange= ifelse(r[i], "reversed", "TRUE")),
                       xaxis = list(title = x_lab))
      col <- 1
      if ("data" %in% names(X[[i]])){
        this_data <- X[[i]]$data
      }else{
        this_data <- X[[i]]
      }
      nc <- ncol(this_data)
      nc <- ifelse(is.null(nc), NA, nc)
      if (is.na(nc)){
        # if it's a univariate timeseries
        facet_list[[i]] <- facet_list[[i]] |>
          plotly::add_lines(name = panel_labels[i],
                            x = times[[i]],
                            y = this_data,
                            line = list(color = line_colors[1]))
      }else{
        # if there are multiple axes
        while (col <= nc){
          facet_list[[i]] <- facet_list[[i]] |>
            plotly::add_lines(name = paste0(panel_labels[i], axis_names[col], sep = ' '),
                              x = times[[i]],
                              y = this_data[,col],
                              line = list(color = line_colors[col]))
          col <- col + 1
        } # end loop over xyz axes
      } # end of 'else if there are several axes'
    } # end loop over sensors
    
    # draw interactive plot
    plot_out <- plotly::subplot(facet_list, 
                                nrows = length(X),
                                heights = panel_heights,
                                shareX = TRUE,
                                titleY = TRUE)
    if (draw){
      plot_out
    }else{
      return(plot_out)
    }
    
  }else{ # end if interactive
    facet_list <- lapply(X = as.list(names(X)), 
                         FUN = plott_static_panel, 
                         sensor_data = X,
                         line_colors = line_colors,
                         panel_labels = panel_labels, 
                         axis_names = axis_names, 
                         times = times, 
                         x_lab = x_lab,
                         r = r)
    
    if (draw){
      cowplot::plot_grid(plotlist = facet_list,
                         align = 'v',
                         ncol = 1,
                         rel_heights = panel_heights)
    }else{
      return(facet_list)
    }
    
  } # end of ggplot static plot 
} # end plott