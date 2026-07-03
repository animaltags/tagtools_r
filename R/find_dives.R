#' Find time cues for dives
#'
#' This function is used to find the time cues for the start and end of either dives in a depth record or flights in an altitude record.
#' @param p A depth or altitude time series (a sensor data list or  a vector) in meters.
#' @param sampling_rate The sampling rate of the sensor data in Hz (samples per second).
#' @param mindepth The threshold in meters at which to recognize a dive or flight. Dives shallower or flights lower than mindepth will be ignored.
#' @param surface The threshold in meters at which the animal is presumed to have reached the surface. Default value is 1. A smaller value can be used if the dive/altitude data are very accurate and you need to detect shallow dives/flights.
#' @param findall When TRUE, forces the algorithm to include incomplete dives at the start and end of the record. Default is FALSE, which only detects and returns complete dives. If findall is TRUE, the start/end times of first/final dives may be NA, indicating the dive started or ended before or after the data recording period.
#' @param mindepth_dur Minimum amount of time in seconds during which mindepth must be exceeded for a dive to be detected. Default is 0 (any excursion where mindepth is exceeded is detected). Note that mindepth_dur is the minimum amount of time where mindepth is exceeded (not the duration of the whole dive/flight).
#' @param na_action How missing data in \code{p} is treated: "keep" or "omit"? "keep", the default, will return NA for the \code{max} if \code{p} has any NA values during the detected dive/flight, and dive/flight start/ends will not be detected when \code{p} is NA. The other option, "omit", is similar to setting na.rm = TRUE and may be appropriate if the data has a small number of missing values that are missing at random. If \code{na_action} is "omit" and there are long runs of missing values in \code{p}, a warning will be issued (the output max depths are probably not accurate in such a case).
#' @return dives is a data frame with one row for each dive/flight found. The columns of dives are: start (time in seconds of the start of each dive/flight), end (time in seconds of the start of each dive/flight), max (maximum depth/altitude reached in each dive/flight), tmax	(time in seconds at which the animal reaches the max depth/altitude).
#' @export
#' @examples
#' BW <- beaked_whale
#' dives <- find_dives(p = BW$P$data, 
#' sampling_rate = BW$P$sampling_rate, 
#' mindepth = 25, surface = 5, 
#' findall = FALSE)

find_dives <- function(p, 
                       mindepth, 
                       sampling_rate = NULL, 
                       surface = 1, 
                       findall = FALSE,
                       mindepth_dur = 0,
                       na_action = "keep") {
  if (is.list(p)) {
    sampling_rate <- p$sampling_rate
    p <- p$data
  } else {
    # p has to be a column vector (a one-col matrix, in R-ese)
    if (is.vector(p)){
      p <- matrix(p, ncol = 1)
    }
    if (nrow(p) == 1) {
      p <- t(p)
    }
    if (is.null(sampling_rate)) {
      stop("sampling_rate is required when p is a vector")
    }
  }
  
  if (!(na_action %in% c("keep", "omit"))){
    stop("Unknown input for na_action - options are 'keep' or 'omit'.")
  }
  
  if (na_action == "omit"){
    # warn the user if na_action is "omit" and there are long runs of missing data
    na_runs <- rle(is.na(as.vector(p)))
    if (max(na_runs$lengths) / sampling_rate > 10){
      # if there's a run of NAs more than 10s long
      warning(paste0("na_action is set to 'omit', but p data contains many missing values (longest series of NAs is ",
                     max(na_runs$lengths) / sampling_rate, 
                     " seconds long). Check data to confirm that ignoring missing values is appropriate!"))
    } 
  }

  searchlen <- 20 # how far to look in seconds to find actual surfacing
  dpthresh <- 0.25 # vertical velocity threshold for surfacing
  dp_lp <- 0.25 # low-pass filter frequency for vertical velocity
  
  # hack for case where there are just a few (<10s) of missing data at start of record
  if (is.na(p[1])){
    # find the index number of the first non-NA entry in p
    start_ix <- min(which(!is.na(p)))
    # as long as there are fewer than 10s of initial NAs...
    if ((start_ix / sampling_rate) < 10){
      # replace the initial NAs with the first non-NA value
      p[1:start_ix] <- p[start_ix]  
    }
  }
  
  # hack for case where first depth obs is > mindepth
  if (p[1] > mindepth){
    p[1] <- mindepth - 0.25
    }

  # find threshold crossings and surface times
  tth <- which(diff(p > mindepth) > 0)
  tsurf <- which(p < surface)
  ton <- 0 * tth
  toff <- ton
  k <- 0
  empty <- integer(0)
  # sort through threshold crossings to find valid dive start and end points
  for (kth in 1:length(tth)) {
    if (all(tth[kth] > toff)) {
      ks0 <- which(tsurf < tth[kth])
      ks1 <- which(tsurf > tth[kth])
      if (findall || ((!identical(ks0, empty)) & (!identical(ks1, empty)))) {
        k <- k + 1
        if (identical(ks0, empty)) {
          ton[k] <- 1
        } else {
          ton[k] <- max(tsurf[ks0])
        }
        if (identical(ks1, empty)) {
          toff[k] <- length(p)
        } else {
          toff[k] <- min(tsurf[ks1])
        }
      }
    }
  }
  # truncate dive list to only dives with starts and stops in the record
  ton <- ton[1:k]
  toff <- toff[1:k]
  # filter vertical velocity to find actual surfacing moments
  n <- round(4 * sampling_rate / dp_lp)
  dp <- fir_nodelay(
    matrix(c(0, diff(p)), ncol = 1) * sampling_rate,
    n, dp_lp / (sampling_rate / 2)
  )
  # for each ton, look back to find last time whale was at the surface
  # for each toff, look forward to find next time whale is at the surface
  
  # allocate space for output
  dives <- data.frame(start = NA * ton,
                      end = NA,
                      max = NA,
                      tmax = NA)
  # keep track of whether duration exceeding mindepth is long enough 
  # (if mindepth_dur = 0, the default, then it always will be so preallocate TRUE)
  mindepth_dur_met <- rep(TRUE, length.out = length(ton))
  for (k in 1:length(ton)) {
    ind <- ton[k] + (-round(searchlen * sampling_rate):0)
    ind <- ind[which(ind > 0)]
    ki <- suppressWarnings(max(which(dp[ind] < dpthresh)))
    if (length(ki) == 0 | is.infinite(ki)) {
      ki <- 1
    }
    ton[k] <- ind[ki]
    ind <- toff[k] + (0:round(searchlen * sampling_rate))
    ind <- ind[which(ind <= length(p))]
    ki <- min(which(dp[ind] > -dpthresh))
    if (length(ki) == 0 | is.infinite(ki)) {
      ki <- 1
    }
    toff[k] <- ind[ki]
    dives$start[k] <- ton[k] / sampling_rate
    dives$end[k] <- toff[k] / sampling_rate
    # find max depth in this dive.
    # if there are NAs and na_action is "keep", then result will be NA
    # if na_action is "omit", then max of non-NA depths will be returned.
    dives$max[k] <- max(p[ton[k]:toff[k]], na.rm = na_action == "omit")
    km <- which.max(p[ton[k]:toff[k]])
    dives$tmax[k] <- (ton[k] + km - 1) / sampling_rate
    # if mindepth_dur is nonzero, check that excursion beyond mindepth was long enough
    if (mindepth_dur > 0){
      # check that whale was at at least mindepth for at least mindepth_dur
      deep_runs <- rle(as.vector(p[ton[k]:toff[k]]) >= mindepth)
      # max run of samples @ > mindepth
      deep_dur_samp <- max(c(0, deep_runs$lengths[deep_runs$values == TRUE]), na.rm = TRUE)
      mindepth_dur_met[k] <- ifelse(deep_dur_samp >= mindepth_dur * sampling_rate, TRUE, FALSE)
    }
  }
  
  if (findall){
    # don't return start/end times for dives that we don't know the actual st/et of
    dives$start[ton == 1] <- NA
    dives$end[toff == length(p)] <- NA
  }
  
  if (mindepth_dur > 0){
    dives <- dives[mindepth_dur_met,]
  }
  
  return(dives)
}