#' Decompose a rotation (or direction cosine) matrix
#'
#' This function is used to decompose a rotation (or direction cosine) matrix into Euler angles, pitch, roll, and heading.
#' @param Q is a 3x3 rotation matrix.
#' @return A 1x3 vector containing: prh=[p,r,h] where p is the pitch angle in radians, r is the roll angle in radians, and h is the heading or yaw angle in radians.
#' @export
#' @examples
#' set <- matrix(c(0.6765458, 0.7227523, 0.1411200,
#'0.3675912, -0.4975063, 0.7857252,
#'0.6380928, -0.4797047, -0.6022632), nrow = 3, ncol = 3)
#' rotmat2euler(set)

rotmat2euler <- function(Q) {
  prh <- c(asin(Q[3, 1]), atan2(Q[3, 2], Q[3, 3]), atan2(Q[2, 1], Q[1, 1]))
  return(prh)
}