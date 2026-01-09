#' Use magnetometer data to estimate the body rotations from cyclic locomotion movements.
#'
#' For derivation see: Martin López L, Aguilar de Soto N, Miller P, Johnson M
#'   2016 Tracking the kinematics of caudal-oscillatory swimming: A comparison 
#'   of two on-animal sensing methods. J Exp Biol 219:2103-2109.
#'
#'	Note: to estimate the stroking rate of small animals for which the specific
#'	acceleration is larger, it may be simpler to find the zero crossings on the 
#'	high-pass-filtered acceleration (z axis for cetaceans, y axis for pinnipeds
#'  and fish) directly rather than using \code{get_br}.
#'
#' @param Ma is the triaxial magnetometer data in the animal frame. Ma can be a sensor structure or a three-column matrix. The sampling rate of Ma must be at least 2x the highest stroke rate of the animal. The magnetometer data can be in any units as long as all three	columns have the same unit.
#' @param sampling_rate is the sampling rate of Ma in Hz, and is only needed if Ma is not a sensor structure.
#' @param fh is the high-pass filter frequency in Hz to use to separate orientation changes from locomotory strokes. It should be about half of the dominant stroke frequency. Use \code{dsf()} to estimate the dominant stroke frequency.
#' @param thr is an optional minimum field strength threshold to prevent errors in the computation. Errors arise if the plane of rotations is nearly perpendicular to the local magnetic field vector. To avoid these, the body rotation signal is replaced with NaN if the field strength in the locomotory plane drops below thr fraction of the total field strength. The default value is 0.2 (i.e., the locomotory plane must have at least 20% of the total field strength to compute the body rotations). 
#' @param axis is an optional indicator that the locomotion is in the x-y plane. The default is \code{axis = 'x'}, that is, the function expects the locomotion to be in the x-z plane (e.g., cetacean swimming) by default. To compute body rotations in the x-y plane (e.g., for pinnipeds and many fish), use \code{ax = 'y'}.                                                                                                                            
#' @return A one-column matrix containing the body rotation signal in radians. It has the same sampling rate and number of samples as Ma.
#' @export
#' @examples
#' ph <- get_br(harbor_seal$M, fh = 0.06, axis = 'y')
#' # choose an angle threshold, e.g., thr = 2 degrees, and find strokes in ph
#' thr <- 2 / 180 * pi
#' zc_result <- zero_crossings(ph, thr, harbor_seal$M$samping_rate / 0.2)
#' # positive-going half strokes
#' ps <- zc_result$K(zc_result$s>0) / harbor_seal$M$samping_rate
#' # negative-going half strokes	
#' ns <- zc_result$K(zc_result$s<0) / harbor_seal$M$samping_rate
#' 

get_br <- function(Ma, sampling_rate, fh, thr = 0.2, axis = 'x') {
  # if Ma is a sensor data structure
  if (is.list(Ma) & "data" %in% names(Ma)) {
    sampling_rate <- Ma$sampling_rate
    Ma <- Ma$data
  }else{
    if (missing(sampling_rate)){
      stop("sampling_rate is a required input if Ma is a matrix.")
    }
  }
  
  if (missing(fh)){
    stop("fh (high-pass filter frequency in Hz) is a required input.")
  }

  # mean magnetic field strength
  mfs <- mean(norm2(Ma), na.rm = TRUE)
  
  # split the M signals into low-pass and high-pass
  Mf <- comp_filt(Ma, sampling_rate, fc = fh)		
  Ml <- Mf[[1]]	# the low-pass filtered M
  Mh <- Mf[[2]]	# the high-pass filtered M
  
  # if body rotations are in x-y plane (like pinnipeds, fish)
  if (axis == 'y'){
    axis_a <- 1 # x
    axis_b <- 2 # y
  # if body rotations are in x-z plane (like whales, dolphins)
  }else{
    axis_a <- 1 # x
    axis_b <- 3 # z
  }
  
  # the magnitude-squared of Ml in the [axis_a, axis_b] sub-space
  m2 <- Ml[,axis_a]^2 + Ml[,axis_b]^2	
  # estimate the body rotations
  # note: matlab asin() essentially never returns NA
  # R's does for all real inputs outside [-1,1] (hence the complex())
  # but there are still a few other inputs that return NA and I don't really know why.
  ph <- Re(asin(
    complex(real = (Mh[,axis_a] * Ml[,axis_b] - Mh[,axis_b] * Ml[,axis_a]) / m2,
            imaginary = 0) 
    ))
 
  # blank out rotations when the planar field is too small
  ph[m2 < (thr * mfs^2)] <- NA
  
  return(ph)
}