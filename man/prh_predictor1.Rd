% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prh_predictor1.R
\name{prh_predictor1}
\alias{prh_predictor1}
\title{Predict the tag position on a diving animal from depth and acceleration data}
\usage{
prh_predictor1(P, A, sampling_rate = NULL, TH = 100, DIR = "descent")
}
\arguments{
\item{P}{is a dive depth vector or sensor structure with units of m H2O.}

\item{A}{is an acceleration matrix or sensor structure with columns ax, ay, and az. Acceleration can be in any consistent unit, e.g., g or m/s^2, and must have the same sampling rate as P.}

\item{sampling_rate}{is the sampling rate of the sensor data in Hz (samples per second). This is only needed if neither A nor M are sensor structures.}

\item{TH}{is an optional minimum dive depth threshold (default is 100m). Only the descents at the start of dives deeper than TH will be analysed (and the ascents at the end of dives deeper than TH if ALL is true).}

\item{DIR}{is an optional dive direction constraint. The default (DIR = 'descent') is to only analyse descents as these tend to give better results. But if DIR = 'both', both descents and ascents are analysed.}
}
\value{
PRH, a data frame with columns \code{cue} \code{p0}, \code{r0}, \code{h0}, and \code{q}
with a row for each dive edge analysed. \code{cue} is the time in second-since-tag-start of the dive edge analysed.
\code{p0}, \code{r0}, and \code{h0} are the deduced tag orientation angles in radians.
\code{q} is the quality indicator with a low value (near 0, e.g., <0.05) indicating that the data fit more consistently with the assumptions of the method.
}
\description{
Predict the tag position on a diving animal parameterized by p0, r0, and
h0, the canonical angles between the principal axes of the tag and the animal.
The tag orientation on the animal can change with time and this function
provides a way to estimate the orientation at the start and end of each suitable
dive. The function critically assumes that the animal rests horizontally at the
surface (at least on average) and dives steeply away from the surface without an
initial roll. If ascents are processed, there must also be no roll in the last
seconds of the ascents. See prh_predictor2 for a method more suitable to animals
that make short dives between respirations.
The function provides a graphical interface showing the estimated tag-to-animal
orientation throughout the deployment. Follow the directions above the top panel
of the figure to edit or delete an orientation estimate.
}
\seealso{
\link{prh_predictor2}, \link{tag2animal}
}
