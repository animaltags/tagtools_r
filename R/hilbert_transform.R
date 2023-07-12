#' Return the Hilbert transform of a signal
#'
#' This function is used to compute the Hilbert transform of a signal. It is based on function HilbertTransform() from (defunct) package hht, which was modified from the EMD package by Donghoh Kim and Hee-Seok Oh (http://dasan.sejong.ac.kr/~dhkim/software.emd.html)
#' @param x The signal vector to be buffered
#' @return The "analytic signal," in other words the Hilbert transform of the input signal x
#' @export
#' @examples timez <- seq(from = 0, by = 1/1024, to = 1)
#' x <- sin(2*pi*60*timez)
#' y <- hilbert_transform(x)
hilbert_transform <- function(x)
{
  #Return the Hilbert transform of a signal.
  #Code modified from the EMD package by Donghoh Kim and Hee-Seok Oh (http://dasan.sejong.ac.kr/~dhkim/software.emd.html)
  #INPUTS
  #    x - the signal to be transformed
  #OUTPUTS
  #    ASIG - the analytic.signal
  ndata <- length(x)
  h <- rep(0, ndata)
  
  if(ndata %% 2 == 0)
  {
    h[c(1, ndata/2+1)] <- 1 
    h[2:(ndata/2)] <- 2 
  }
  else
  {
    h[1] <- 1
    h[2:((ndata + 1)/2)] <- 2 
  }
  
  asig <- stats::fft(h * stats::fft(x), inverse = TRUE)/ndata
  invisible(asig)
} 