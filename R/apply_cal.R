#' Implement a calibration on tag sensor data
#'
#' Given an appropriate set of calibration constants and information, this function will apply the calibration procedure to a tag sensor data set. Cal fields currently supported are: poly, cross, map, tcomp, tref
#'
#' @param X A tag sensor data list, or a matrix or vector containing tag sensor data
#' @param sampling_rate of X in Hz. Only required (and only used) if X is *not* a sensor data list
#' @param cal A calibration list for the data in X from, for example, \code{\link{spherical_cal}}.
#' @param Tempr a tag sensor data list or a vector of temperature measurements for use in temperature compensation. Tempr is assumed to have the same sampling duration and sampling rate as the data in \code{X}. Tempr is only required (and only used) if there is a tcomp item in the \code{cal} list.
#' @param nomap logical. If TRUE, axis mapping for vector sensors is disabled. Default: FALSE, to apply axis mapping if one is specified in \code{cal}.
#'
#' @return A tag sensor data structure (or a matrix or vector, if X was a matrix or vector) with the calibration implemented. Data size and sampling rate are the same as for the input data \code{X}, but units may have changed. \code{cal} elements currently supported are: poly, cross, map, tcomp, tref, tseg, tcomp. Any other elements in \code{cal} will be ignored.
#' @export
#' @examples
#' A_cal <- apply_cal(harbor_seal$A, cal = spherical_cal(harbor_seal$A$data))
#' 

apply_cal <- function(X,
                      sampling_rate,
                      cal, 
                      Tempr = NULL,
                      nomap = FALSE) {
  if (!is.list(cal)) {
    stop("Input argument cal must be a calibration list (for example, from spherical_cal)")
  }

  if (is.list(X)) {
    x <- X$data
    sampling_rate <- X$sampling_rate
    if (!is.matrix(x)) {
      x <- matrix(x, ncol = 1)
    }
    if (length(x) == 0) {
      stop("No data found in input X")
    }
  } else {
    x <- X
    if (missing(sampling_rate) & "tcomp" %in% tolower(names(cal))){
      stop("input sampling_rate is required for tcomp calibration")
    }
  }

  if (is.list(Tempr)) {
    tfs <- Tempr$sampling_rate
    Tempr <- Tempr$data
    if (!is.matrix(Tempr)) {
      Tempr <- matrix(Tempr, ncol = 1)
    }
    if (nrow(Tempr) != nrow(x)) {
      Tempr <- interp2length(Tempr, x, fs_in = tfs, fs_out = sampling_rate)
    }
  }

  k <- which("tseg" == tolower(names(cal)))[1]
  if (!is.na(k)){
    kseg <- round(cal[[k]] * sampling_rate) + 1
    kseg_max <- sapply(kseg, FUN = function(z) max(c(z, 1)))
    kseg <- data.frame(st = sapply(kseg_max, FUN = function(z) min(z , nrow(x)-1), x = x))
    kseg$et <- c(utils::tail(kseg$st, -1) - 1, nrow(x))
    if (is.list(X)){
      if ("data" %in% names(X)){
        X$cal_tseg = cal[[k]]
      }
    }else{
      kseg <- data.frame(st = 1,
                         et = nrow(x))
    }
  }
  
  # find and apply the calibration polynomial
  k <- which("poly" == tolower(names(cal)))[1]
  if (!is.na(k)) {
    p <- cal[[k]]
    if (nrow(p) != ncol(x)) {
      em <- paste("Calibration polynomial must have",
        ncol(x), " rows to match the number of columns in input data X",
        sep = ""
      )
      stop(em)
    }
    # check for time-varying poly
    if (is.na(dim(p)[3]) | dim(p)[3] == nrow(kseg)){
      for (kk in c(1:nrow(kseg))){
        ks <- c(kseg[kk,1] : kseg[kk,2])
        x[ks,] <- x[ks,] * matrix(p[,1,kk], nrow = length(ks), ncol = nrow(p), byrow = TRUE) +
          matrix(p[,2,kk], nrow = length(ks), ncol = nrow(p), byrow = TRUE)
      }
    }else{
      x <- x * matrix(p[,1], nrow = nrow(x), ncol = nrow(p), byrow = TRUE) +
        matrix(p[,2], nrow = nrow(x), ncol = nrow(p), byrow = TRUE)
    }
    if (is.list(X)) {
      if ("data" %in% names(X)){
        X$cal_poly <- matrix(cal$poly, nrow = nrow(cal$poly))        
      }
    }
  } # end of "if poly"

  # find and apply temperature compensation
  k <- which("tcomp" == tolower(names(cal)))[1]
  if (!is.null(Tempr) & !is.na(k)) {
    p <- matrix(cal$tcomp, nrow = ncol(x))
    k <- which("tref" == tolower(names(cal)))[1]
    tref <- matrix(0, nrow = 1, ncol = ncol(Tempr))
    if (!is.na(k)){
      tref[,1 : length(cal[[k]])] <- cal[[k]]
    }
    Tempr <- remove_nan(Tempr - matrix(tref, nrow = nrow(T), ncol = ncol(tref), byrow = TRUE))
    
    k <- which("tconst" == tolower(names(cal)))[1]
    if (!is.na(k)){
      tc <- matrix(0, nrow = 1, ncol = ncol(Tempr))
      tc[, 1:length(cal[[k]])] <- cal[[k]]
      for (kk in c(1:ncol(Tempr))){
        if (tc[kk] < 0){
          next         
        }
        # pole frequency of a one-pole low-pass filter
        pf <- 1 / (sampling_rate * tc[kk])
        Tempr[,kk] <- gsignal::filter(pf, 
                                      a = 1 - (1-pf),
                                      x = Tempr[,kk],
                                      zi = Tempr[1,kk])
        
      }
      if (is.list(X)){
        if ("data" %in% names(X)){
          X$cal_tconst <- tc
        }
      }
    }
    
    k <- which("tadvance" == tolower(names(cal)))[1]
    if (!is.na(k)){
      ta <- matrix(0, nrow = 1, ncol = ncol(Tempr))
      ta[, 1:length(cal[[k]])] <- cal[[k]]
      for (kk in c(1:ncol(Tempr))){
        if (ta[kk] < 0){
          next
        }
        nd <- round(sampling_rate * ta[kk])
        Tempr[,kk] <- rbind(Tempr[c(nd:nrow(Tempr)), kk],
                            matrix(Tempr[nrow(Tempr), kk],
                                   nrow = nd-1,
                                   ncol = 1))
      }
      if (is.list(X)){
        if ("data" %in% names(X)){
          X$cal_tadvance <- ta
        }
      }
    } # end of tadvance
    
    # test for time-varying tcomp
    if (ncol(p) / ncol(Tempr) == nrow(kseg)){
      for (kk in c(1:nrow(kseg))){
        ks <- c(kseg[kk,1] : kseg[kk,2])
        pp <- p[,(kk-1) * ncol(Tempr) + c(1:ncol(Tempr))]
        x[ks,] <- x[ks,] + Tempr[ks,] %*% t(p)
      }
    }else{
      x <- x + Tempr %*% t(p)
    }
  } # end of tcomp

  # find and apply any cross-axis corrections - only for vector sensors
  k <- which("cross" == tolower(names(cal)))[1]
  if (!is.na(k)){
    p <- cal[[k]]
    # test for time-varying cross
    if (dim(p)[3] == nrow(kseg)){
      for (kk in c(1:nrow(kseg))){
        ks <- c(kseg[kk,1] : kseg[kk,2])
        x[ks,] <- x[ks,] %*% p[,,kk]
      }
    }else{
      x <- x %*% p
    }
    if (is.list(X)) {
      if ("data" %in% names(X)){
        X$cal_cross <- matrix(cal$cross, nrow = nrow(cal$cross))
      }
    }
  } # end of cross

  # find and apply an axis conversion map - only for vector sensors
  if (!nomap){
    k <- which("map" == tolower(names(cal)))[1]
    if (!is.na(k)){
      p <- cal[[k]]
      x <- x %*% p
      if (is.list(X)) {
        if ("data" %in% names(X)){
          X$cal_map <- p
          if ("axes" %in% names(cal)){
            X$axes = cal$axes
          }
        }
      } # end of "if X is a sensor list
    } # end of map
  } # end of "if !nomap"
  
  if (!is.list(X)) {
    X <- x
    return(X)
  }

  X$data <- x
  X$frame <- "tag"

  if ("unit" %in% names(cal)) {
    X$source_unit <- X$unit
    X$source_unit_name <- X$unit_name
    X$source_unit_label <- X$unit_label
    X$unit <- cal$unit
    X$unit_name <- cal$unit_name
    X$unit_label <- cal$unit_label
  }

  if ("name" %in% names(cal)) {
    X$cal_name <- cal$name
  }

  if (!("history" %in% names(X)) | is.null(X$history)) {
    X$history <- "apply_cal"
  } else {
    X$history <- paste(X$history, "apply_cal", sep = ",")
  }
  return(X)
}

#' @rdname apply_cal
#' @aliases apply_cal
#' @note Matlab animaltag tools have function do_cal, while older DTAG Matlab tools have apply_cal. All function similarly, so we've written \code{\link{apply_cal}} to have all the functionality of the newest \code{do_cal} from Matlab, and added an alias (so if you run \code{do_cal()} in R, it just calls \code{apply_cal()}).
#' @examples do_cal(harbor_seal$A, cal = spherical_cal(harbor_seal$A$data))
#' @export
do_cal <- apply_cal
