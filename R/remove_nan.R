#' Remove missing values
#'
#'  Replace any NaNs in the columns of X with the nearest non-NA number in the same column. If an entire column is NA, the first non-NA number in the matrix is used as a filler. If the entire matrix is NA, 1 is used as a filler.
#' @param x vector or matrix of numeric values

#' @note Thanks to:  https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#' @return x with NAs replaced as noted above
#' @export

remove_nan <- function(x) {
  if (is.matrix(x)){
    x <- apply(x, MARGIN = 2, FUN = remove_nan_vec)
  }else{
    x <- remove_nan_vec(x)
  }
  if (all(is.na(x))){
    if (is.matrix(x)){
      x <- matrix(1, nrow = nrow(x), ncol = ncol(x))
    }else{
      x <- rep.int(1, times = length(x))
    }
  }
  return(x)
} 

remove_nan_vec <- function(x){
  non_na <- !is.na(x)
  x <- x[which(non_na)[c(1,1:sum(non_na))][cumsum(non_na)+1]]
}
