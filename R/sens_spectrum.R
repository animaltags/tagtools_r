#' Spectrum level of a sensor time series, i.e., the amount of power per 1 Hz band.
#'
#' This replicates the function of Matlab's \code{psd} function (now deprecated in Matlab). 
#' The input signal is divided into overlapping pieces equal in length to the required Fast Fourier Transform (FFT) length.
#' Each piece is windowed and the FFT computed. 
#' The spectral power is then estimated from the mean of the spectral magnitudes squared. 
#' Power is scaled to account for the scale factor of the FFT and the window.
#' The power is also scaled by 10log10 of the bin width in Hz (i.e., the sampling rate divided by the FFT length) to convert the per-bin powers into approximate per-Hz powers.
#' @param X a sensor list or a vector or matrix containing the sensor signals to be processed. For multi-axial sensors when X is not a sensor list, each axis should be in a column of X.
#' @param sampling_rate is the sampling rate of the signals in X in Hz. This is only needed when X is not a sensor list.
#' @param nfft The length of the fft to use. Choose a power of two for fastest operation. Default value is 512.
#' @param pct optionally sets the percentage level used to compute the lower and upper spectra. The default value is 1 percent, which is suitable for 10s averages if there is at least 1 day of data. If the data duration is less than a day, pct should be increased accordingly if accurate spectra are required.
#' @param compute_ul Logical: whether to compute and output \code{Pu} and \code{Pl}, which give power in the intervals with highest and lowest activity. Default: TRUE
#' @return A list including variables described below (u/l variables are only present if input \code{compute_ul} is TRUE.) P, Pu, and Pl will have the same number of columns as the input data X, if X is a matrix. Other elements are vectors whose length matches the number of frequencies, and the number of rows in P, Pu, Pl.
#' \itemize{
#' \item{\strong{P: }} is the spectral power at each frequency in power per Hz. The spectrum is single-sided and extends to \code{sampling_rate}/2. The spectrum is scaled so that the power in the signal is the same as the total power in the spectrum, i.e., \code{sampling_rate / nfft * sum(colSums(P))} should be equal to: \code{mean(colSums(X^2))}.
#' \item{\strong{freq: }} The vector of frequencies in Hz at which P is calculated.
#' \item{\strong{Pl: }} is the spectral power of nfft-length intervals with the lowest activity. Pl is scaled in the same way as P. 
#' \item{\strong{Pu: }} is the spectral power of nfft-length intervals with the highest activity. Pl is scaled in the same way as P. 
#' \item{\strong{tl: }} central times of the intervals listed in Pl
#' \item{\strong{tu: }} central times of the intervals listed in Pu
#' }
#' @export
#' @examples 
#' spect_data <- sens_spectrum(beaked_whale$M, nfft = 8)
sens_spectrum <- function(X, 
                          sampling_rate = NULL,
                          nfft = 512, 
                          pct = 1,
                          compute_ul = TRUE) {
  if (missing(X)){
    stop("Cannot compute sensor spectrum without data input X")
  }
  
  if (is.list(X)){
    if (utils::hasName(X, "data")){
      # if animaltags sensor list is input
      sampling_rate <- X$sampling_rate
      X <- X$data
    } else {
      # try to coerce data frame input to matrix
      X <- as.matrix(X)
    }
  }
  
  # if X is vector, make it a column matrix
  if (!is.matrix(X)){
    X <- matrix(X, ncol = 1)
  }else{
    if (nrow(X) == 1){
      X <- matrix(X, ncol = 1)
    }
  }
  
  nh <- round(nfft/2)
  nov <- nh
  w <- gsignal::hanning(nfft)
  
  if (compute_ul){
    # identify high activity intervals by summed variance of each block
    blk_std <- block_std(X, n = nfft, nov = nov)
    if (ncol(blk_std$sd) > 1){
      S <- rowSums(blk_std$sd^2)  
    }else{
      S <- sum(blk_std$sd^2)
    }
    if (sum(is.na(S)) == length(S)){
      stop("Error trying to identify high- and low-activity intervals -- do your data have many missing values?")
    }
    Ssort <- sort(S, index.return = TRUE)
    # Ssort$x is the values of S sorted ascending with NAs at the end
    # Ssort$ix is the indices in S of Ssort$x
    npct <- round(length(S) * pct / 100)
    kl <- Ssort$ix[c(1:npct)]
    ku <- utils::tail(Ssort$ix[!is.na(Ssort$ix)], npct)
  }
  
  # preallocate space for output
  sens_spec_out <- 
    c(
    lapply(c(1 : ifelse(compute_ul, 3, 1)), function(zz) matrix(data = 0, nrow = nh, ncol = max(c(1, ncol(X))))),
    lapply(c(1 : ifelse(compute_ul, 3, 1)), function(zz) matrix(data = 0, nrow = nh, ncol = 1)))
  sso0 <- lapply(c(1 : ifelse(compute_ul, 3, 1)), function(zz) matrix(data = 0, nrow = 1, ncol = max(c(1, ncol(X)))))
  if (compute_ul){
    names(sens_spec_out) <- c("P", "Pu", "Pl", "freq", "tu", "tl")
    names(sso0) <- c("P0", "P0l", "P0u")
  }else{
    names(sens_spec_out) <- c("P", "freq")
    names(sso0) <- c("P0")
  }
  
  for (k in c(1:ncol(X))){
    # loop over columns of X
    Xbuff <- buffer(X[,k], n = nfft, overlap = nov, nodelay = TRUE)
    mX <- colMeans(Xbuff)
    XX <- (Xbuff - matrix(mX, nrow = nrow(Xbuff), ncol = length(mX), byrow = TRUE)) *
      matrix(w, nrow = length(w), ncol = ncol(Xbuff), byrow = FALSE)
    # note: n rows in XX SHOULD be nfft so no need to truncate XX here (which would be the R equiv of inputting nfft to matlab's fft())
    ff <- stats::mvfft(XX) # mvfft applies the fft to the columns of XX
    F <- Re(ff[c(1:nh), ] * Conj(ff[c(1:nh), ]))
    # find the indices of the columns in F that have no NA values
    kk <- which(apply(!is.na(F), MARGIN = 2, FUN = all))
    if (length(kk) == 0){ stop("NA values in fft output")} # not sure when this would happen?
    sens_spec_out$P[,k] <- rowMeans(F[,kk])
    sso0$P0[1, k] <- mean(mX[kk]^2)
    if (compute_ul){
      sens_spec_out$Pl[,k] <- rowMeans(F[,kl])
      sens_spec_out$Pu[,k] <- rowMeans(F[,ku])
      sso0$P0l[1,k] <- mean(mX[kl]^2)
      sso0$P0u[1,k] <- mean(mX[ku]^2)
    }
  }
  
  # The following scaling steps give correct output for simulated input
  # Multiplying by 2 adds power in the top half of the spectrum.
  # Dividing by nfft^2 corrects the nfft scaling in matlab's fft. -- DO WE ALSO NEED THIS IN R?
  # Dividing by sampling_rate/nfft changes scaling from power per bin to power per Hz.
  # Dividing by sum(w.^2)/nfft corrects for the window.

  slc <- 2 / (sampling_rate * sum(w^2))
  sens_spec_out$P <- sens_spec_out$P * slc
  
  # Scale the 0-frequency value differently as it was calculated outside of the FFT to
  # avoid the window function:
  # Dividing by fs/nfft changes scaling from power per bin to power per Hz.
  sens_spec_out$P[1, ] <- sso0$P0 * (nfft / sampling_rate) ;
  
  sens_spec_out$freq <- c(0 : ((nfft/2) - 1)) / nfft * sampling_rate
  
  if (compute_ul){
    sens_spec_out$Pl <- sens_spec_out$Pl[c(1:floor(nfft/2)), ] * slc
    sens_spec_out$Pu <- sens_spec_out$Pu[c(1:floor(nfft/2)), ] * slc
    sens_spec_out$tl <- sort(kl) * nov / sampling_rate
    sens_spec_out$tu <- sort(ku) * nov / sampling_rate
  }
  
  return(sens_spec_out)
}
