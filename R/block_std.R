#' Compute standard deviation of sample blocks
#'
#' This function is used to compute the standard deviations of successive blocks of samples.
#' @param X A vector or a matrix containing samples of a signal in each column.
#' @param n The number of samples from X to use in each analysis block.
#' @param nov (optional) The number of samples that the next block overlaps the previous block. The default value is 0.
#' @return A list with 2 elements:
#' \itemize{
#'  \item{\strong{sd: }} A vector or matrix containing the standard deviation value computed for each block. If X is a mxn matrix, Y is pxn where p is the number of complete n-length blocks with nov that can be made out of m samples, i.e., n+(p-1)*(n-nov) < m
#'  \item{\strong{samples: }} The time at which each output in Y is reported, in units of samples of X.  So if samples[1] = 12, then the value Y[1] corresponds to the “time” 12 samples in X.
#' }
#' @export
#' @examples sample_matrix <- matrix(c(1, 3, 5, 7, 9, 11, 13, 15, 17), byrow = TRUE, ncol = 3)
#' my_block_sds <- block_std(sample_matrix, n = 3, nov = 1)
block_std <- function(X, 
                      n, 
                      nov = 0) {
  nov <- min(n, nov)
  
  # try to coerce data.frame input to matrix
  if (is.data.frame(X)){
    X <- as.matrix(X)
  }
  
  # if X is a row vector make it a column
  if (nrow(X) == 1) {
    X <- t(X)
  }
  
  if (is.vector(X) | ncol(X) == 1) {
    # if X is one col stored as an R vector (no orientation implied in R)
    Y <- col_block_std(X, n, nov)
    samples <- round(n / 2 + (0:(length(Y) - 1)) * (n - nov))
  }else{
    # if X is a matrix with one or more columns
    Y <- apply(X, MARGIN = 2, FUN = function(k) col_block_std(k, n, nov), simplify = TRUE)
    # catch the case where p is 1 (only one row of output / only 1 complete block of n)
    if (length(Y) == ncol(X)){
      Y <- matrix(Y, ncol = ncol(X))
    }
    samples <- round(n / 2 + (0:(nrow(Y) - 1)) * (n - nov))
    }
  return(list(sd = Y, samples = samples))
}

col_block_std <- function(col, n, nov){
  ss <- buffer(col, n, nov, nodelay = TRUE)
  Y <- apply(ss, MARGIN = 2, FUN = stats::sd)
  return(Y)
}
