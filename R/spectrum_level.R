#' Compute the spectrum level of a signal x.
#'
#' This function is used to compute the spectrum level of a signal x.
#' @param x A vector containing the signal to be processed. For signals with multiple channels, each channel should be in a column of x.
#' @param nfft The length of the fft to use. Choose a power of two for fastest operation. Default value is 512.
#' @param sampling_rate The sampling rate of x in Hz. Default value is 1. sampling_rate is the vector of frequencies at which SL is calculated.
#' @param w The window length. The default value is nfft. If w<nfft, each segment of w samples is zero-padded to nfft.
#' @param nov The number of samples to overlap each segment. The default value is half of the window length.
#' @return A list with 2 elements:
#' \itemize{
#' \item{\strong{SL: }}The spectrum level at each frequency in dB RMS re root-Hz. The spectrum is single-sided and extends to sampling_rate/2. The reference level is 1.0 (i.e., white noise with unit variance will have a spectrum level of 3-10*log10(sampling_rate). The 3dB is because both the negative and positive spectra are added together so that the total power in the signal is the same as the total power in the spectrum.
#' \item{\strong{freq: }} The vector of frequencies at which SL is calculated.
#' }
#' @note The spectrum is single-sided and extends to sampling_rate/2. The reference level is 1.0 (i.e., white noise with unit variance will have a spectrum level of 3-10*log10(sampling_rate). The 3dB is because both the negative and positive spectra are added together so that the total power in the signal is the same as the total power in the spectrum.
#' @export
#' @examples
#' list <- spectrum_level(x = beaked_whale$P$data, 
#' nfft = 4, sampling_rate = beaked_whale$P$sampling_rate)
#'
spectrum_level <- function(x, nfft = 512, sampling_rate = 1, w = nfft, nov = round(w / 2)) {
  wind <- gsignal::hanning((w + 2))
  wind <- wind[2:(length(wind) - 1)]

  if (!is.null(ncol(x))) {
    xdim <- ncol(x)
  }
  else {
    xdim <- 1
  }
  
  P <- matrix(0, nrow = nfft / 2, ncol = xdim)
  
  for (k in 1:xdim) {
    if (!is.matrix(x)) {
      X <- buffer(x[], length(wind), nov, nodelay = TRUE)
    }
    else {
      X <- buffer(x[, k], length(wind), nov, nodelay = TRUE)
    }
    X <- pracma::detrend(X) * matrix(wind, nrow = length(wind), ncol = ncol(X), byrow = FALSE)
    
    Freq <- abs( apply(X, MARGIN = 2, FUN = stats::fft) )^2
    P[, k] <- rowSums(Freq[1:(nfft / 2), ])
  }
  ndt <- ncol(X)
  # these two lines give correct output for randn input
  # SL of randn should be -10*log10(sampling_rate/2)
  slc <- 3 - 10 * log10(sampling_rate / nfft) - 10 * log10(sum(wind^2) / nfft)
  # 3 is to go from a double-sided spectrum to a single-sided (positive frequency) spectrum.
  # sampling_rate/nfft is to go from power per bin to power per Hz
  # sum(wind^2)/nfft corrects for the window
  SL <- 10 * log10(P) - 10 * log10(ndt) - 20 * log10(nfft) + slc
  # 10*log10(ndt) corrects for the number of spectra summed in P (i.e., turns the sum into a mean)
  # 20*log10(nfft) corrects the nfft scaling in matlab's fft
  freq <- (c(0:((nfft / 2) - 1))) / nfft * sampling_rate
  return(list(SL = SL, freq = freq))
}
