#' Correct a depth or altitude profile for offsets caused by miscalibration and sensor drift.
#'
#' This function finds minima in the dive/altitude profile that are consistent with surfacing/landing and smooths these to make a time-varying '0 pressure' offset vector. This tool should be used if there are still pressure offsets after correcting for temperature using manufacturer black-box calibration or e.g. \code{\link{fix_pressure}}.
#' @param p A sensor list or vector of depth/altitude in meters
#' @param sampling_rate The sampling_rate of p in Hz. This is only needed if p is not a sensor list.
#' @param interval is the search interval in seconds that is used to find surfacings or landings. This should be chosen to be a little more than the usual inter-surfacing/inter-landing interval. Default value is 1 hour.
#' @param tc is the smoothing time constant in seconds used to filter the surface depth offsets. \code{tc} should normally be several times larger than \code{interval} unless the depth/altitude sensor has a fast drift. Default value is 12 hours. If \code{tc} is less than or equal to \code{interval}, no smoothing is performed.
#' 
#' @return A list with 2 elements:
#' \itemize{
#'  \item{\strong{p: }} A sensor list or vector of corrected depth/altitude measurements at the same sampling rate as the input data. If the input is a sensor list, the output will also be.
#'  \item{\strong{offsets: }} is a 2-variable data frame containing a set of \code{times} (column 1) and estimated \code{pressure_offset}s (column 2). Times are in seconds since the first sample in p. Pressure offsets are in metres.
#' }
#' @note This function makes a number of assumptions about the depth/altitude data and about the behaviour of animals: First, the depth data should have few incorrect outlier (negative) values that fall well beyond the surface. These can be reduced using \code{\link{median_filter}} before calling \code{fix_offset_pressure}. the pressure offset in the sensor varies slowly and smoothly with respect to the inter-surfacing/inter-landing interval. This function will not be effective at correcting step changes in calibration.
#' @export

fix_offset_pressure <- function(p, 
                                sampling_rate, 
                                interval = 3600, # 1 hr = 3600s 
                                tc = 12*3600 # 12 hr in s
                                ) {

  if (missing(p)) {
    stop("fix_offset_pressure requires input pressure data p")
  }
  
  
  if ("data" %in% names(p)){
    P <- p
    sampling_rate <- p$sampling_rate
    p <- p$data
  } else {
    if (missing(sampling_rate)) {
      stop("sampling_rate is a required input when p is not a sensor data list")
    }
    P <- NULL
  }
  
  # decimate depth to around 5Hz if sampled faster than that
  if (sampling_rate > 5) {
    df <- round(sampling_rate / 5)
    pp <- decdc(pp, df) 
    fsd <- sampling_rate / df
  }else{
    pp <- p
    fsd <- sampling_rate
  }
  
  kinterval <- round(interval * fsd / 2)
  p_buff <- buffer(pp,
                   n = 2 * kinterval,
                   overlap = kinterval,
                   nodelay = TRUE)
  offs <- apply(p_buff, MARGIN = 2, FUN = min, na.rm = TRUE)

  offs <- remove_nan(offs)
  
  # smooth the local offsets
  offs <- matrix(offs, ncol = 1)
  if (nrow(offs) > 3){
    offs <- median_filter(offs, n = 3)
  }
  
  # added condition not in matlab: data must be long enough that offs is longer than FIR length
  if (tc <= interval | length(offs) < round(fsd / fc)){
    fof <- offs
  }else{
    fc <- interval / tc / 2
    fof <- fir_nodelay(offs, 
                       n = round(fsd / fc), # note: matlab uses fs (not fsd) here - is that a mistake?
                       fc)
  }
  
  T <- c(1:nrow(fof)) * kinterval / fsd
  poffs <- stats::approx(x = c(0, T, length(p) / sampling_rate),
                         y = c(fof[1], fof, utils::tail(fof, 1)),
                         xout = c(1:length(p)) / sampling_rate)$y
  p <- p - poffs
  offsets <- data.frame(times = T,
                        pressure_offsets = fof)
  
  if (is.null(P)){
    # input not a sensor data list
    return(list(p = p, offsets = offsets))
  }else{
    # input WAS sensor data list (so return one, modified)
    P$data <- p
    P$cal_offset = offsets$pressure_offsets
    P$cal_tseg = offsets$times
    if (!("history" %in% names(P))){
      P$history <- 'fix_offset_pressure'
    }else{
      P$history <- paste(P$history, "fix_offset_pressure", sep = ",")
    }
    return(list(p = P, offsets = offsets))
  }
}
