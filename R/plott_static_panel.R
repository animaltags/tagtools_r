#' Helper function for plott
#'
#' This internal function helps\code{plott} produce the individual panels that make up the static ggplot output. It will not usually be needed by users.
#'
#' @param sensor Which sensor to plot
#' @param sensor_data List whose elements are either lists (containing data and metadata) or vectors/matrices of time series data. See details.
#' @param line_colors Vector of colors to use for lines
#' @param panel_labels Labels for each panel (sensor)
#' @param axis_names Names for different axes for sensors with more than one (for example: X, Y, Z)
#' @param times List of vectors of times for the x axis of each panel (one list element per panel)
#' @param x_lab Title for x axis (string)
#' @param r whether or not to reverse the y axis scale for each sensor
#' @return A ggplot for "one panel," that is, with the data from one sensor

plott_static_panel <- function(sensor, sensor_data, line_colors, panel_labels, axis_names = c("X", "Y", "Z"), times, x_lab, r) {
  
  if (missing(line_colors)) {
    line_colors <- c("#000000", "#009E73", "#9ad0f3", "#0072B2", "#e79f00", "#D55E00")
  }
  if (missing(panel_labels)) {
    panel_labels <- ""
  }
  
  names(panel_labels) <- names(sensor_data)
  names(r) <- names(sensor_data)
  names(times) <- names(sensor_data)
  named_axis_colors <- line_colors
  color_legend_names <- paste(panel_labels[sensor], axis_names, sep = ' ')
  names(named_axis_colors) <- color_legend_names
  
  # initialize empty panel
  this_plot <- ggplot2::ggplot() +
    # add x and y axis labels
    ggplot2::labs(x = x_lab, y = panel_labels[sensor])
  # reverse y axis if needed
  if (r[sensor]){
    this_plot <- this_plot + ggplot2::scale_y_reverse()
  }
  # check if data is a list of vectors/matrices or if it's an animaltags list object
  # pull out the single timeseries to be plotted either way
  if ("data" %in% names(sensor_data[[sensor]])){
    this_data <- sensor_data[[sensor]]$data
  }else{
    this_data <- sensor_data[[sensor]]
  }
  nc <- ncol(this_data)
  if(is.null(nc)){
    nc <- 1
  }
  if(is.na(nc)){
    nc <- 1
  }
  
  this_long_data <- as.numeric(matrix(this_data, ncol = 1, byrow = FALSE))
  this_plot <- this_plot +
    ggplot2::geom_path(mapping = ggplot2::aes(x = rep(times[[sensor]], times = nc),
                                              y = this_long_data,
                                              col = rep(color_legend_names[1:nc], each = length(times[[sensor]]))))
  # add legend with axis names
  if (nc > 1){
    this_plot <- this_plot +
      ggplot2::scale_color_manual('', values = named_axis_colors) +
      ggplot2::guides(col = ggplot2::guide_legend(title = '',
                                                  position = 'inside')) +
      ggplot2::theme(
        legend.key = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.direction = 'horizontal',
        legend.position.inside = c(1,1),
        legend.justification.inside = c(1,1)
      )
  } else { # end of if nc > 1
    this_plot <- this_plot +
      ggplot2::scale_color_manual('', values = named_axis_colors) +
      ggplot2::theme(legend.position = 'none')
  }
return(this_plot)
  
} # end plott_static_panel