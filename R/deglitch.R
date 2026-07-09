#' Remove single-sample outliers from sensor data vector
#' 
#' @param x numeric data vector
#' @param m Multiplicative factor by which the first difference of the sample data must exceed the mean first difference, to be replaced with preceding value. Default: 10.
#' @param reps Number of times to run the detection iteratively (when outliers are replaces, the mean first difference will change...) Default: 4

#' @return x, with single-sample outliers replaced by the preceding value ("outliers" as defined above)
#' @export

deglitch <- function(x,
                     m = 10,
                     reps = 4){
  for (rep in c(1:reps)){
    d1 <- abs(diff(x))
    outlier_ix <- which(d1 > (mean(d1, na.rm = TRUE) * m))
    if (length(outlier_ix) == 0){break}
    if (outlier_ix[1] == 1){
      x[1:2] <- NA
      outlier_ix <- utils::tail(outlier_ix, -1)
      if (length(outlier_ix) == 0){break}
    }
    x[outlier_ix] <- x[outlier_ix - 1]
  }
  return(x)
} # end of deglitch

