#' Least-squares solver for 3-dimensional data-driven calibration of field sensors. 
#' 
#' This function is called by auto_cal_acc and auto_cal_mag. The function uses a locally linearized least-squares formulation so should be run iteratively several times until converged.
#'
#' @param X a 3-column data matrix representing measurements of a field vector (i.e., a constant norm). X may be affected by various calibration errors and by additive noise. The objective of this function is to infer the calibration errors in X so as to return an improved estimate of the correct field vector measurements. 
#' @param G is the initial calibration matrix that was used to generate the incoming X. If X is uncalibrated, leave G at its default value.
#' @param method of calibration:  1 for offset only (the default), 2 for offset and gain, 3 for offset, gain, and cross-terms; or 4 for offset, gain, cross-terms, and an auxiliary covariate.
#' @param Aux A matrix of (optional) auxiliary co-variate(s) (e.g., temperature or pressure measurements) with the same number of rows as X. Each column in Aux is a covariate. If multiple covariates are provided, they should be reasonably uncorrelated to avoid numerical problems.
#'
#' @return A list with 2 elements:
#' \itemize{
#' \item{\strong{X: }} the improved data matrix after calibration errors have been corrected
#' \item{\strong{G: }}  a matrix of calibration corrections. The first three columns form a 3x3 matrix of gains and cross-terms. If method=1, this matrix will be the identity matrix. If method=2, the matrix will be diagonal. The next column of G is a vector of offsets. If an  auxiliary covariate is given, G will have an additional two columns for each column in Aux comprising the scale factor and offset of the covariate for each axis of X. If an input G is given, the output G will contain both the input and output calibrations as a single compound calibration.
#' }
#' @export
#' @examples 
#' lssolve3(beaked_whale$A$data)
lssolve3 <- function(X,
                     G = diag(nrow = 3, ncol = 4),
                     method = 1,
                     Aux = NULL) {
  # scaling to control condition
  Xscf <- 2 * mean(abs(X), na.rm = TRUE)
  X <- X * (1/Xscf)
  
  kg <- which(apply(X, function(x) all(!is.na(x)), MARGIN = 1))
  norig <- nrow(X)
  X <- X[kg, ]
  bsq <- matrix(rowSums(X^2), ncol = 1)
  XX <- cbind(2*X, matrix(1, nrow = length(kg), ncol = 1))
  
  if (!is.null(Aux)){
    Auxu <- Aux[kg, ]
    mAux <- colMeans(Auxu)
    # pivot the temperature to keep condition down
    Aux <- Auxu - matrix(mAux, nrow = nrow(Auxu), ncol = length(mAux), byrow = TRUE)
    Auxscf <- 1 / colMeans(abs(Aux))
    Aux <- Aux * matrix(Auxscf, nrow = nrow(Aux), ncol = length(Auxscf), byrow = TRUE)
    for (k in seq(from = ncol(Aux), by = -1, to = 1)){
      XX <- cbind(2 * X * matrix(Aux[,k], nrow = nrow(Aux), ncol = 3, byrow = FALSE), XX)
    }
  }
  
  if (method > 1){
    XX <- cbind(2 * X[, c(1:2)]^2, XX)
    if (method >= 3){
      XX <- cbind(2 * cbind(X[,1] * X[,2],
                            X[,1] * X[,3],
                            X[,2] * X[,3]),
                  XX)
    }
  }
  
  # formulate and solve the least squares equation
  RR <- t(XX) %*% XX
  P <- matrix(colSums(matrix(bsq, nrow = length(bsq), ncol = ncol(XX)) * XX),
              nrow = 1, byrow = TRUE)
  H <- - solve(RR) %*% t(P)
  R <- diag(3)

  # interpret the results
  if (method > 1){
    if (method >=3){
      # % distribute the cross-terms between the axes:
      # % the distribution is done so as to allow G to be factored
      # % into a diagonal gain matrix and a symmetric cross-term matrix
      # % to match the way that cross-terms are applied by do_cal
      gg <- matrix(1 + c(H[4:5], 0), ncol = 1)
      cc <- matrix(H[1:3], ncol = 1) / matrix(c(gg[1] + gg[2],
                                                gg[1] + gg[3],
                                                gg[2] + gg[3]),
                                              ncol = 1)
      R <- R + matrix(c(0, cc[1:2], cc[1], 0, cc[3], cc[2:3], 0),
                      ncol = 3,
                      nrow = 3,
                      byrow = TRUE)
      H <- matrix(H[4 : nrow(H)], ncol = 1)
    }
    # try to ensure that cross terms stay symmetric
    sg <- diag(sqrt(1 + c(H[1:2], 0)), nrow = 3, ncol = 3)
    # prior matlab version was: R = diag(1+[H(1:2);0])*R, current is R = sg*R*sg
    R <- sg %*% R %*% sg
    H <- matrix(H[3:nrow(H)], ncol = 1)
    X <- X %*% R
    G[, 1:3] <- G[, 1:3] %*% R
  }
  
  H <- matrix(utils::head(H, -1) * Xscf, nrow = 3, byrow = FALSE)
  X <- X * Xscf
  
  if (!is.null(Aux)){
    # correct for covariate scaling
    HH <- H[, c(1:ncol(Aux)) * matrix(Auxscf, nrow = 3, ncol = length(Auxscf), byrow = TRUE)]
    H <- matrix(H[, ncol(Aux) + 1], ncol = 1)
    if (ncol(G) > 4){
      G[, 4 + c(1:ncol(Aux))] <- R %*% G[, 4 + c(1:ncol(Aux))] + HH
    }else{
      G[, 4 + c(1:ncol(Aux))] <- HH
    }
    X <- X + Auxu %*% t(HH)
    # correct for covariate mean removal
    H <- H - colSums(HH * matrix(mAux, nrow = 3, ncol = length(mAux), byrow = TRUE))
  }
  
  G[,4] <- R %*% matrix(G[,4], ncol = 1) + H
  X <- X + matrix(H, nrow = length(kg), ncol = length(H), byrow = TRUE)
  Y <- matrix(NA, nrow = norig, ncol = 3)
  Y[kg, ] <- X
  return(list(X = Y, G = G))
}