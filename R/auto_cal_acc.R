#' Data-driven calibration of triaxial accelerometer data
#'
#'This function low-pass filters the accelerometer data to reduce the specific acceleration and then performs a constrained least-squares fit to the constant gravitational field strength. In effect, the function adjusts the calibration on each axis so that the data
#'points in A fall as close as possible to a sphere centred at the origin and with radius 9.81 m/s^2.
#'
#' @param A is an accelerometer sensor structure or matrix with columns [ax ay az]. Acceleration should be in m/s^2
#' @param sampling_rate Sampling rate of A in Hz. Only required if A is not a sensor data list.
#' @param cal A calibration list for the data in X. Only list elements "poly" and "cross" are supported. If cal is given, the function will try to improve it. If no cal is given, the function tries to infer the cal from the data. 
#' @param use is an optional vector whose length matches the number of rows as in \code{A}. It is used to tell \code{auto_cal_acc} which data points to use. Only data points for which the corresponding row of \code{use} is > 0 are used. If \code{use} is not given, all data points in \code{A} are used.
#' @param fa target analysis sampling rate in Hz. Recommendation: use 0.5 for large animals (great whale) and 5 for small animals (dolphin, porpoise). Defaults to 5.
#' @param jerk_pct jerk selection threshold as a percentage. A small value removes a lot of data points. If your dataset is small/short, or your species is not very active, you may need to increase \code{jerk_pct} from the default value of 10.
#' @param do_crop logical; default is TRUE. Include an option to crop the data before doing the data-based calibration calculations? Data to be used for calibration should exclude periods where the tag is not on the animal. If you need to exclude non-contiguous segments through the record, you might use input \code{use} instead.
#'
#' @note This algorithm has been tested extensively on DTAG and SMRT data but not on data from other tags. If it doesn't work well for your data, let us know - it may help us improve the tool.
#' @return A list with entries:
#' \itemize{
#' \item{\strong{A: }} the improved accelerometer sensor structure or matrix. It has the same data rate as the input data and is in m/s^2. 
#' \item{\strong{cal: }}  the improved calibration structure.
#' }
#' @export
#' @examples
#' A_cal <- auto_cal_acc(harbor_seal$A,spherical_cal(harbor_seal$A$data))
#' 

auto_cal_acc <- function(A,
                         sampling_rate,
                         cal,
                         use = NULL,
                         fa = 5,
                         jerk_pct = 10,
                         do_crop = TRUE) {
  if (missing(cal)){
    cal <- list(poly = matrix(c(1, 0, 1, 0, 1, 0), nrow = 3, byrow = TRUE))
  }
  
  if (is.list(A)) {
    if ("data" %in% names(A)){
      sampling_rate <- A$sampling_rate
      Ad <- A$data      
    }
    if (!is.matrix(Ad)) {
      Ad <- matrix(Ad, ncol = 1)
    }
    if (length(Ad) == 0) {
      stop("No data found in input A")
    }
  } else {
    Ad <- A
  }
  
  # find where A is changing rapidly
  J <- diff(Ad)^2
  J <- rowSums(rbind(J, utils::tail(J,1)))
  
  if (!is.null(use)){
    Ad <- Ad[use > 0, ]
    J <- J[use > 0, ]
    do_crop <- FALSE # can't crop if data is pasted together based on use; use says which times to use!
  }

  if (fs > fa){
    df <- ceiling(fs / fa)
    Ad <- decdc(Ad, df)
    fsd <- fs / df
    J = abs(decdc(J, df)) # decimate jerk too
  }else{
    fsd <- fs
  }
  
  fstr <- 9.81 # earth's gravitational acceleration in m/s2
  if (do_crop){
    crop_out <- crop(Ad, fsd) # crop Ad via GUI
    tc <- crop_out$tcues
    Ad <- crop_out$Y
    rm(crop_out)
    J <- crop_to(J, sampling_rate = fsd, tcues = tc) # apply same crop to J
  }
  
  # note: matlab changes from cal$POLY to cal$poly here but R apply_cal is not case specific
  
  thr <- stats::quantile(J, probs = jerk_pct / 100)
  AA <- Ad[J < thr, ]
  # working at line 126 of Matlab version. Need to write spherical_ls()
  

}