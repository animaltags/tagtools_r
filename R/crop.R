#' Interactive data cropping tool.
#'
#' This function plots the input data # and allows the user to select start and end times for cropping.
#'
#' Possible input combinations include: crop(X) if X is a sensor list, crop(X, sampling_rate) if X is a vector or matrix.
#' @param X A sensor list, vector or matrix. X can be regularly or irregularly sampled data in any frame and unit.
#' @param sampling_rate The sampling rate of X in Hz. This is only needed if X is not a sensor list. If X is regularly sampled, sampling_rate is one number.
#' @param times A vector of sampling times for X. This is only needed if X is not a sensor list and X is not regularly sampled.
#' @param quiet If quiet is false, print to the screen
#' @return A list with 3 elements:
#' \itemize{
#'  \item{\strong{Y: }} A sensor list, vector or matrix containing the cropped data segment. If the input is a sensor list, the output will also be. The output has the same units, frame and sampling characteristics as the input.
#'  \item{\strong{times: }} A vector of sampling times for Y. This is only returned if X is irregularly sampled and X is not a sensor list. If X is a sensor list, the sampling times are stored in the list.
#'  \item{\strong{tcues: }} tcues is a two-element vector containing the start and end time cue in seconds of the data segment kept, i.e., tcues = c(start_time, end_time).
#' }
#' @examples data <- beaked_whale
#' Pc <- crop(data$P, quiet=TRUE) 
#' Ydata <- Pc$data
#' plot(-Ydata)
#' @export

crop <- function(X, sampling_rate = NULL, times = NULL, quiet=FALSE) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  
  if (missing(X)) {
    stop("X is a required input")
  }
  if (is.list(X)) {
    x <- X$data
    sampling_rate <- X$sampling_rate
    if (is.null(x)) {
      stop("data cannot be empty")
    }
    if (!is.matrix(x)) {
      x <- matrix(x, ncol = 1)
    }
  } else {
    if (missing(sampling_rate)) {
      stop("inputs for X and sampling_rate are required")
    }
    x <- X
    if (!is.matrix(x)) {
      x <- matrix(x, ncol = 1)
    }
    if (nrow(x) == 1) {
      x <- matrix(x, nrow = length(x), ncol = 1)
    }
  }
  if (!is.null(times)) {
    graphics::matplot(times, x, type = "p", xlab = "Time (seconds)")
    graphics::matplot(times, x, type = "p", xlab = "Time (seconds)", ylim = c(floor(graphics::par("usr")[3]), ceiling(graphics::par("usr")[4])))
  } else {
    graphics::matplot((c(1:nrow(x)) / sampling_rate), x, type = "l", xlab = "Time (seconds)")
    graphics::matplot((c(1:nrow(x)) / sampling_rate), x, type = "l", xlab = "Time (seconds)", ylim = c(floor(graphics::par("usr")[3]), ceiling(graphics::par("usr")[4])))
  }
  if (!quiet){
    print("Position your cursor and then click once followed by clicking FINISH to change the start, or click twice in the same spot followed by clicking FINISH to change the end. If you wish to change both the start and end click once at the start time desired and twice at the end time desired.")
  }
  if (length(times) > 1) {
    tcues <- c(min(times), max(times))
  } else {
    tcues <- c(0, (nrow(x) / sampling_rate))
  }
  LIMS <- tcues
  pts <- graphics::locator(3) # This should probably be a shiny app at some point in order to make the interactiveness more user friendly
  if (length(pts$x) == 1) {
    tt <- pts$x
    tcues[1] <- max(LIMS[1], min(tt, tcues[2]))
    graphics::abline(v = (c(1, 1) * tcues[1]), col = "green", lwd = 1.5)
    graphics::points(tcues[1], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "green", pch = 6)
    graphics::abline(v = (c(1, 1) * tcues[2]), col = "red", lwd = 1.5)
    graphics::points(tcues[2], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "red", pch = 6)
  } else {
    if (length(pts$x) == 2) {
      tt <- pts$x[2]
      tcues[2] <- min(LIMS[2], max(tt, tcues[1]))
      graphics::abline(v = (c(1, 1) * tcues[1]), col = "green", lwd = 1.5)
      graphics::points(tcues[1], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "green", pch = 6)
      graphics::abline(v = (c(1, 1) * tcues[2]), col = "red", lwd = 1.5)
      graphics::points(tcues[2], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "red", pch = 6)
    } else {
      if (length(pts$x) == 3) {
        ts <- pts$x[1]
        tcues[1] <- max(LIMS[1], min(ts, tcues[2]))
        graphics::abline(v = (c(1, 1) * tcues[1]), col = "green", lwd = 1.5)
        graphics::points(tcues[1], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "green", pch = 6)
        te <- pts$x[3]
        tcues[2] <- min(LIMS[2], max(te, tcues[1]))
        graphics::abline(v = (c(1, 1) * tcues[2]), col = "red", lwd = 1.5)
        graphics::points(tcues[2], mean(c((graphics::par("usr")[3]), (graphics::par("usr")[4]))), col = "red", pch = 6)
      }
    }
  }
  if (is.list(X)) {
    cto <- crop_to(X, tcues = tcues)
    Y <- cto
    return(Y)
  } else {
    cto <- crop_to(x, sampling_rate = sampling_rate, tcues = tcues)
    if (!is.null(times)) {
      Y <- cto$X
      times <- cto$times
      return(list(Y = Y, times = times, tcues = tcues))
    } else {
      Y <- cto
      return(list(Y = Y, tcues = tcues))
    }
  }
}
