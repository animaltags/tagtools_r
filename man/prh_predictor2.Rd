% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prh_predictor2.R
\name{prh_predictor2}
\alias{prh_predictor2}
\title{Predict the tag position on a diving animal from depth and acceleration data}
\usage{
prh_predictor2(P, A, sampling_rate = NULL, MAXD = 10)
}
\arguments{
\item{P}{is a dive depth vector or sensor structure with units of m H2O.}

\item{A}{is an acceleration matrix or sensor structure with columns ax, ay, and az. Acceleration can be in any consistent unit, e.g., g or m/s^2, and must have the same sampling rate as P.}

\item{sampling_rate}{is the sampling rate of the sensor data in Hz (samples per second). This is only needed if neither A nor M are sensor structures.}

\item{MAXD}{is the optional maximum depth of near-surface dives. The default value is 10 m. This is used to find contiguous surface intervals suitable for analysis.}
}
\value{
PRH, a data frame with columns \code{cue} \code{p0}, \code{r0}, \code{h0}, and \code{q}
with a row for each dive edge analysed. \code{cue} is the time in second-since-tag-start of the dive edge analysed.
\code{p0}, \code{r0}, and \code{h0} are the deduced tag orientation angles in radians.
\code{q} is the quality indicator with a low value (near 0, e.g., <0.05) indicating that the data fit more consistently with the assumptions of the method.
}
\description{
Predict the tag position on a diving animal parametrized by p0, r0, and
h0, the canonical angles between the principal axes of the tag and the animal.
The tag orientation on the animal can change with time and this function
provides a way to estimate the orientation at the start and end of each suitable
dive. The function critically assumes that the animal makes a sequence of short
dives between respirations and that the animal remains upright (i.e., does not roll)
during these shallow dives. See prh_predictor1 for a method more suitable to animals
that rest horizontally at the surface. The function provides a graphical interface showing the estimated tag-to-animal
orientation throughout the deployment. Follow the directions above the top panel
of the figure to edit or delete an orientation estimate.
The function provides a graphical interface showing the estimated tag-to-animal
orientation throughout the deployment. Follow the directions above the top panel
of the figure to edit or delete an orientation estimate.
}
\seealso{
\link{prh_predictor1}, \link{tag2animal}
}
