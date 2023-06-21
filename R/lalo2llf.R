#' Convert latitude-longitude track points into a local level frame
#'
#' @param trk A data frame, two-column matrix, two-element vector  of track points c(latitude, longitude) or sensor data  structure.
#' @param pt c(latitude, longitude) of the centre point of the local level frame. If pt is not given, the first point in the track will be used.
#' @return A data frame with columns \code{northing} and \code{easting} of track points in the local level frame. Northing and easting are in metres. The axes of the frame are true (geographic) north and true east.
#' @export
#' @examples
#' coordinates <- matrix(c(
#'-122.4194, 37.7749,
#'-73.9352,  40.7306), nrow = 2, ncol = 2, byrow = TRUE)
#' lalo2llf(coordinates, c(15,19))
#' @note This function assumes the track is on the surface of the geoid,
#'  and also uses a simple spherical model for the geoid. For
#'  more accurate conversion to a Cartesian frame, use spatial and mapping packages in Matlab/Octave.
lalo2llf <- function(trk, pt = NULL) {
  
  if (is.list(trk)) {
    if ('data' %in% names(trk) & 'depid' %in% names(trk)){
      trk <- trk$data[,c(2:3)] }
    
    if (is.data.frame(trk)) {
      trk <- as.matrix(trk)
    } else {
      trk <- as.matrix(as.data.frame(trk))
    }
}
  
  
  if (is.null(pt)) {
      pt <- trk[1, ]
    } 

  trk <- trk - matrix(data = pt,nrow=nrow(trk), ncol = 2, byrow= TRUE)
    
  
  NE <- data.frame(northing = as.double(trk[, 1]) * 1852 * 60,
                    easting = as.double(trk[, 2]) * 1852 * 60 * cos(as.double(pt[1]) * pi / 180)
  )
  
  return(NE)
}