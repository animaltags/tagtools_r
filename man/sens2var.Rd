% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sens2var.R
\name{sens2var}
\alias{sens2var}
\title{Extract data from a sensor structure.}
\usage{
sens2var(Sx, Sy = NULL, regular = NULL)
}
\arguments{
\item{Sx}{A tag sensor data structure (for example, an entry in an animaltag object returned by \code{\link{load_nc}}).}

\item{Sy}{(optional) A second tag sensor data structure. Include if you want to check two tag sensor data streams for compatibility in terms of duration and sampling rate.}

\item{regular}{(optional) Logical. Default is FALSE. Should \code{Sx} be checked to see whether it was regularly sampled?}
}
\value{
A list with entries:
\itemize{
\item{\code{X: }} {Data vector or matrix from sensor \code{Sx}, with the same units of measure and sampling rate as in \code{Sx}. NULL if \code{regular} is TRUE and the data \code{Sx} are not regularly sampled, or if \code{Sx} is not a tag sensor data list.}
\item{\code{Y: }} {Data vector or matrix from sensor \code{Sy}, with the same units of measure and sampling rate as in \code{Sx}.  NULL unless \code{Sy} was input, and NULL if \code{Sy} is not a tag sensor data list.}
\item{\code{fs: }} {sampling rate of the sensor data in Hz (samples per second), if \code{Sx} was regularly sampled.}
\item{\code{t: }} {Times (in seconds) of irregularly sampled data. The time reference (i.e., the 0 time) is with respect to the start time of the data in the sensor structure. NULL unless \code{Sx} had irregular sampling and \code{Sx} is a tag sensor data list.}
\item{\code{sampling: }} {NULL unless input \code{regular} is TRUE. 'regular' if sampling of \code{Sx} was regular, 'irregular' if it was irregular.}
}
}
\description{
This function extracts loose data variables from tag sensor data lists.
It can also be used to check two sensor data lists for compatibility (i.e., same duration and sampling rate).
}
\note{
This function is provided for the compatibility checking functionality and for parallel operation with the matlab/octave tag toolkit. However, in R loose variables in the workspace are less commonly used in R, and an R function does not return multiple objects as matlab functions can. Thus, the result of "unpacking" a sensor list is the same data...stored in different and less well documented list (pretty useless).
}
\examples{
# no example given because hard to figure out when you'll use it!
}
