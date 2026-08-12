#' Deduce the calibration constants
#' 
#' Least squares solver for spherical data-driven calibration. This function is used by \code{\link{auto_cal_acc}} and \code{auto_cal_mag}, and is a wrapper function for \code{\link{lssolve3}}.
#' @param X a 3-column data matrix representing measurements of a field vector (i.e., a constant norm). X may be affected by various calibration errors and by additive noise. The objective of this function is to infer the calibration errors in X so as to return an improved estimate of the correct field vector measurements.
#' @param field_strength is the target field strength in the same units as X.
#' @param cal is an optional list of calibration information. Only cal$poly and cal$cross are supported.
#' @param method of calibration:  1 for offset only (the default), 2 for offset and gain, 3 for offset, gain, and cross-terms; or 4 for offset, gain, cross-terms, and an auxiliary covariate.
#' @param Aux A matrix of (optional) auxiliary co-variate(s) (e.g., temperature or pressure measurements) with the same number of rows as X. Each column in Aux is a covariate. If multiple covariates are provided, they should be reasonably uncorrelated to avoid numerical problems.
#' 
#' @return A list with elements:
#' \itemize{
#' \item{\strong{X: }} the improved data matrix after calibration errors have been corrected
#' \item{\strong{cal: }} the improved calibration list
#' \item{\strong{sigma: }}  a two-element vector reporting the standard deviation of field strength in the data, before and after the data-driven calibration
#' }
#' @export
#' @examples
#' slso <- spherical_ls(beaked_whale$A$data, 9.81)

spherical_ls <- function(X, 
                         field_strength = NULL, 
                         cal,
                         method = 1,
                         Aux = NULL) {
  if (! method %in% c(1, 2, 3, 4)){
    stop("Unrecognized method. Options are: 1 for offset only (the default), 2 for offset and gain, 3 for offset, gain, and cross-terms; or 4 for offset, gain, cross-terms, and an auxiliary covariate.")
  }
  
  if (missing(cal)){
    g <- diag(1, nrow = 3, ncol = 4)
    cal <- list()
  }else{
    g <- cbind(diag(cal$poly[,1]), matrix(cal$poly[,2], ncol = 1))
  }
  
  nn <- norm2(X)
  sigma <- vector(mode = "numeric", length = 2)
  sigma[1] <- stats::sd(nn, na.rm = TRUE) / mean(nn, na.rm = TRUE)
  # apply initial cal
  XX <- X %*% g[, c(1:3)] + matrix(g[,4], nrow = nrow(X), ncol = nrow(g))
  
  lss_out <- lssolve3(XX, G = g, method = method, Aux = Aux)
  for (k in c(1:3)){
    # repeat 4x for convergence
    lss_out <- lssolve3(lss_out$X, G = lss_out$G, method = method, Aux = Aux)
  }
  
  # apply cross terms symmetrically
  cr <- solve(diag(diag(g))) %*% g[,c(1:3)]
  
  if (!missing(field_strength)){
    scf <- field_strength / mean(norm2(XX), na.rm = TRUE)
    XX <- XX * scf
  }else{
    scf <- 1
  }
  
  cal$cross <- solve(diag(diag(g))) %*% g[, c(1:3)]
  g[,4] <- t(solve(cal$cross)) %*% matrix(g[, 4], ncol = 1)
  cal$poly <- scf * cbind(matrix(diag(g), ncol = 1), matrix(g[, 4], ncol = 1))
  
  if (!is.null(Aux)){
    cal$tcomp <- scf * t(solve(cal$cross)) %*% g[, c(5:nrow(g))]
  }
  
  nn <- norm2(XX)
  sigma[2] <- stats::sd(nn, na.rm = TRUE) / mean(nn, na.rm = TRUE)
  
  return(list(X = XX, cal = cal, sigma = sigma))
  
} # end of spherical_ls()
