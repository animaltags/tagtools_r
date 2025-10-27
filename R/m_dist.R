#' Calculate Mahalanobis distance 
#' 
#' This function is used to calculate the Mahalanobis distance for a multivariate time series.
#' @param data A data frame or matrix with one row for each time point. Note that the Mahalanobis distance calculation should be carried out on continuous data only, so if your data contain logical, factor or character data, proceed at your own risk...errors (or at least meaningless results) will probably ensue.
#' @param sampling_rate The sampling rate in Hz (data should be regularly sampled). If not specified it will be assumed to be 1 Hz.
#' @param smooth_dur The length, in minutes, of the window to use for calculation of "comparison" values. If not specified or zero (the default), there will be no smoothing (a distance will be calculated for each data observation).
#' @param overlap The amount of overlap, in minutes, between consecutive "comparison" windows. \code{smooth_dur} - \code{overlap} will give the time resolution of the output distance time series. Default is 0, which means no overlap.  Overlap will also be set to zero if \code{smooth_dur} is unspecified or zero.
#' @param consec Logical (default FALSE). If \code{consec} is TRUE, then the calculated distances are between *consecutive windows* of duration \code{smooth_dur}, sliding forward over the data set by a time step of (\code{smooth_dur} - \code{overlap}) minutes. Default is \code{consec} = FALSE, which means each output distance will be the distance between the current "comparison" window and the baseline window. If \code{consec} is TRUE, \code{bl_start} and \code{bl_end} inputs will be used *only* to define the period used to calculate the data covariance matrix.  
#' @param cum_sum Logical (default FALSE). If \code{cum_sum} is TRUE, then output will be the cumulative sum of the calculated distances, rather than the distances themselves. Default is cum_sum = FALSE.
#' @param bl_start Start time (in seconds since start of the data set) of the baseline period. The mean data values for this period will be used as the 'control' to which all "comparison" data points (or windows) will be compared. If not specified, it will be assumed to be 0 (start of record). If \code{consec} is TRUE, then adjacent windows will be compared, and this input will have no effect except to define the data used to compute the covariance matrix, if \code{bl_cov} is TRUE.
#' @param bl_end End time (in seconds since start of the data set) of the baseline period. If not specified, the entire data set will be used (baseline_end will be the last sampled time-point in the data set). If consec = TRUE, then adjacent windows will be compared, and this input will have no effect except to define the data used to compute the covariance matrix, if \code{bl_cov} is TRUE.
#' @param bl_cov Logical.  If bl_cov is TRUE, then a covariance matrix using all data *in the baseline period* will be used for calculating the Mahalanobis distance. Default is bl_cov = FALSE, which uses *all* data (in the entire dataset) to compute the covariance matrix.
#' @return Data frame containing results: variable \code{seconds} is times (in seconds since start of dataset) at which Mahalanobis distances are reported. If a \code{smooth_dur} window was applied, then the reported times will be the midpoint of each "comparison" window. Variable \code{dist} contains the computed Mahalanobis distances.
#' @export
#' @examples BW <- beaked_whale
#' m_dist_result <- m_dist(BW$A$data, BW$A$sampling_rate)
#' 


m_dist <- function(data, sampling_rate = 1, smooth_dur = 0, overlap = 0 ,
                   consec = FALSE, cum_sum = FALSE, 
                   bl_start = 0, bl_end = floor(nrow(data)/sampling_rate), 
                   bl_cov = FALSE) {
  # Input checking
  if (smooth_dur == 0 & overlap != 0) {
    overlap <- 0
    warning("overlap has been set to 0, because smooth_dur is 0.")
  }

  # preliminaries - conversion, preallocate space, etc.
  # es <- floor(sampling_rate * expStart) + 1                              # start of experimental period in samples
  # ee <- ceiling(sampling_rate * expEnd)                                  # end of experimental period in samples
  
  # start of baseline period in samples
  bs <- floor(sampling_rate * bl_start) + 1
  # end of baseline period in samples
  be <- min(ceiling(sampling_rate * bl_end), nrow(data))
  # window length in samples
  W <- max(1, smooth_dur * sampling_rate * 60)
  # overlap between subsequent window, in samples
  O <- overlap * sampling_rate * 60
  # number of start points at which to position the window -- start points are W-O samples apart
  N <- ceiling(nrow(data) / (W - O))
  # index over windows, as colum vector 
  k <- matrix(c(1:N), ncol = 1)
  # start times of comparison windows, in samples
  ss <- (k - 1) * (W - O) + 1
  # mid points of comparison windows, in samples (times at which distances will be reported)
  ps <- ((k - 1) * (W - O) + 1) + smooth_dur * sampling_rate * 60 / 2
  # mid-point times in seconds
  mid_t <- ps / sampling_rate
  # mean values during baseline period
  ctr <- colMeans(data[bs:be,], na.rm = TRUE)
  # covariance matrix
  if (bl_cov) {
    # covariance matrix using all data in baseline period
    bcov <- stats::cov(data[bs:be,], use = "complete.obs")           
  } else {
    # covariance matrix using all data
    bcov <- stats::cov(data, use = "complete.obs")
  }
  
  # helper function: alternate way of calculating Mdist
  Ma <- function(d,Sx) { 
    # d is a row vector of pairwise differences between the things you're comparing
    # Sx is the inverse of the cov matrix
    sum((d %*% Sx) %*% d)
  }
  
  # compute distances  
  if (!consec) { # 
    # compute means in each window (potentially with overlap)
    comps <- zoo::rollapply(data, width = W, mean, 
                            by = W - O, by.column = TRUE, align = "left", 
                            fill = NULL, partial = TRUE, na.rm = TRUE) 
    # compute distances between each window and the "baseline" period
    d2 <- apply(comps, MARGIN = 1, FUN = stats::mahalanobis, 
                cov = bcov, center = ctr, inverted = FALSE)
  } else { # to compute distances between consecutive windows (if consec is TRUE)
    # inverse of the baseline cov matrix
    i_bcov <- solve(bcov) 
    # compute means in each window (potentially with overlap)
    ctls <- zoo::rollapply(data, width = W, mean, 
                           by = W-O, by.column = TRUE, align = "left", 
                           fill = NULL, partial = TRUE, na.rm = TRUE) 
    # we will compare values from a given control window with the following window.
    comps <- rbind( ctls[2:nrow(ctls),] , NA * vector(mode = "numeric",length = ncol(data)) ) 
    pair_difsampling_rate <- as.matrix(ctls - comps)
    # compute distances
    d2 <- apply(pair_difsampling_rate, MARGIN = 1, FUN = Ma, Sx = i_bcov)
    d2 <- c(NA, d2[1:(length(d2) - 1)]) # first dist should be at midpoint of first comp window
  }

  
  # functions return squared Mahalanobis dist so take sqrt
  dist <- sqrt(d2)
  dist[mid_t > (nrow(data) / sampling_rate - smooth_dur * 60)] <- NA
  
  # Calculate cumsum of distances if requested
  if(cum_sum) {
    dist <- cumsum(dist)
  }
  
  D <- data.frame(seconds = mid_t, dist)
  return(D)
}
