#' Integrate track with reference positions
#'
#' Simple track integration method to merge infrequent
#' but accurate positions with a regularly sampled track
#' that is not absolutely accurate.
#' @param P a two column matrix or data frame containing the anchor positions. The first column should be the "northing" and the second the "easting" coordinates. (If data frame is input, then columns with those two names, in any position, will be used if present).
#' @param times a vector of times corresponding to the positions P. If P is a data frame with a column called "times" then that column will be used.
#' Times are in seconds since the start of the regularly sampled track.
#' times must have the same number of rows as P. Times
#' must be greater than or equal to 0 and less than the time length of the regularly sampled track.
#' @param D a two column matrix containing the regularly sampled track
#' points. If D is a data frame with columns named 'northing' and 'easting' those will be used regardless of position; otherwise the first column will be northing and the second easting. The two columns contain the 'x' and 'y' coordinates of the
#' track points in a local level frame. Units, axes and frame must match those of P.
#' @param sampling_rate is the sampling rate in Hz of D.
#' @return D, a data frame with 4 columns: "northing" and "easting" along the new track,
#' and "current_n" and "current_e", the track increments needed to match the tracks.
#' If the difference between the two tracks is due to the medium moving,
#' these increments can be considered an estimate of the current in m/s.
#' The axes and frame are the same as for the input data.
#' @export

fit_tracks <- function(P, times = NULL, D, sampling_rate) {
  #*************************
  # input checks
  #*************************
  if ("data.frame" %in% class(P)) {
    if ("times" %in% names(P)) {
      times <- P[["times"]]
    }
    if ("northing" %in% tolower(names(P)) &
        "easting" %in% tolower(names(P))) {
      P <- cbind(P[, "northing"], P[, "easting"])
    }
  }
  
  if (!is.matrix(times) & !is.data.frame(times)) {
    times <- matrix(times, ncol = 1)
  }
  
  if (is.null(times)) {
    stop("fit_tracks: input times is required.\n")
  }
  
  if ("data.frame" %in% class(D)) {
    if ("northing" %in% tolower(names(D)) &
        "easting" %in% tolower(names(D))) {
      D <- cbind(D[, "northing"], D[, "easting"])
    }
  }
  
  #*************************
  # end of input checks
  #*************************
  
  # find position fixes that coincide in time with the DR track
  kg <- which(times >= 0 & times < nrow(D) / sampling_rate)
  # find the corresponding DR track sample numbers
  k <- round(times[kg] * sampling_rate) + 1
  # errors between fixes and DR track at fix times
  V <- rbind(
    c(0, 0),
    P[kg, ] - D[k, ]
  )
  # repeat last error - this will be applied to the remnant DR track after last fix
  V <- rbind(
    V,
    V[nrow(V), ]
  )
  
  dk <- c(k[1], diff(k), nrow(D) - utils::tail(k, 1))
  ki <- c(0, t(cumsum(dk)))
  C <- matrix(0, nrow = nrow(D), ncol = 2) # make space for the merged track
  
  for (kk in c(1:length(dk))) {
    C[(ki[kk] + 1):ki[kk + 1], ] <- matrix(as.numeric(V[kk, ]),
                                           nrow = dk[kk],
                                           ncol = 2,
                                           byrow = TRUE
    ) +
      (matrix(matrix((1 / dk[kk]) * c(0:(dk[kk] - 1)),
                     nrow = dk[kk],
                     ncol = 2
      ) %*%
        as.numeric(V[kk + 1, ] - V[kk, ]),
      nrow = dk[kk],
      ncol = 2
      ))
  }
  
  D <- D + C
  C <- rbind(
    matrix(0, nrow = 1, ncol = ncol(C)),
    diff(C) * sampling_rate
  ) # estimated 'currents'
  D <- data.frame(cbind(D, C))
  names(D) <- c("northing", "easting", "current_n", "current_e")
  return(D)
}
