% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/body_axes.R
\name{body_axes}
\alias{body_axes}
\title{Generate the cardinal axes of an animal}
\usage{
body_axes(A, M, sampling_rate = NULL, fc = NULL)
}
\arguments{
\item{A}{The acceleration matrix with columns [ax ay az], or a sensor data list. Acceleration can be in any consistent unit, e.g., g or $m/s^2$.}

\item{M}{The magnetometer signal matrix, M=[mx,my,mz], or a sensor data list, in any consistent unit (e.g., in uT or Gauss).}

\item{sampling_rate}{sampling rate of A and M in Hz (optional if A and M are sensor data lists)}

\item{fc}{(optional) The cut-off frequency of a low-pass filter to apply to A and M before computing the axes. The filter cut-off frequency is in Hz. The filter length is 4*fs/fc. Filtering adds no group delay. If fc is not specified, no filtering  is performed.}
}
\value{
W, a list with entries \code{x}, \code{y}, and \code{z}; each is an nx3 matrix of body axes where n is the number of rows in M and A.
W$x is a nx3 matrix (or a length-3 vector if A and M have one row) containing the X or longitudinal (caudo-rostral) axes.
W$y is a nx3 matrix (or a length-3 vector if A and M have one row) containing the Y or transverse (left-right) axes.
W$z is a nx3 matrix (or a length-3 vector if A and M have one row) containing the Z or ventro-dorsal axes.
W$sampling_rate has the sampling rate of the A and M.
}
\description{
This function is used to generate the cardinal axes of an animal (i.e., the longitudinal, transverse, and ventro-dorsal) from accelerometer and magnetic field measurements. This functions generates an approximate orthonormal basis from each measurement of A and M by: (i) normalizing A and M to unit length, (ii) rotating the magnetometer measurement to the horizontal plane (Mh), (iii) computing the cross-product, N, of A and Mh to generate the third axis, (iv) transposing [Mh,N,A] to form the body axis basis.
}
\note{
Output sampling rate is the same as the input sampling rate. Irregularly sampled data can be used, but then filtering must not be applied (\code{fc = NULL}).

Frame: This function assumes a [north,east,up] navigation frame and a [forward,right,up] local frame. This function will only return the animal's cardinal axes if the tag was attached so that the sensor axes aligned with the animal's axes OR if the tag A and M measurements are rotated to account for the orientation of the tag on the animal. Otherwise, the axes returned by this function will be the cardinal axes of the tag, not the animal.
}
\examples{
samplematrix1 <- matrix(c(7, 2, 3, 6, 4, 9), byrow = TRUE, ncol = 3)
samplematrix2 <- matrix(c(6, 5, 3, 4, 8, 9), byrow = TRUE, ncol = 3)
W <- body_axes(A = samplematrix1, M = samplematrix2, fc = NULL)
}
