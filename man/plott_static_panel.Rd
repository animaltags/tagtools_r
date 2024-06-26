% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plott_static_panel.R
\name{plott_static_panel}
\alias{plott_static_panel}
\title{Helper function for plott}
\usage{
plott_static_panel(
  sensor,
  sensor_data,
  line_colors,
  panel_labels,
  axis_names = c("X", "Y", "Z"),
  times,
  x_lab,
  r
)
}
\arguments{
\item{sensor}{Which sensor to plot}

\item{sensor_data}{List whose elements are either lists (containing data and metadata) or vectors/matrices of time series data. See details.}

\item{line_colors}{Vector of colors to use for lines}

\item{panel_labels}{Labels for each panel (sensor)}

\item{axis_names}{Names for different axes for sensors with more than one (for example: X, Y, Z)}

\item{times}{List of vectors of times for the x axis of each panel (one list element per panel)}

\item{x_lab}{Title for x axis (string)}

\item{r}{whether or not to reverse the y axis scale for each sensor}
}
\value{
A ggplot for "one panel," that is, with the data from one sensor
}
\description{
This internal function helps\code{plott} produce the individual panels that make up the static ggplot output. It will not usually be needed by users.
}
