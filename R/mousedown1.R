#' Return (x,y) of mouse click position on plot
#'
#' This is an internal function used by prh_predictor (and perhaps other interactive plots). It works with getGraphicsEvent() and prints to screen the x, y location on a plot where the mouse is clicked.
#' @param buttons The keyboard button pressed
#' @param x The x location of the mouse (in ndc coordinates)
#' @param y The y location of the mouse (in ndc coordinates)
#' @param quiet If quiet is false, print to screen
#' @noRd
#'
mousedown1 <- function(buttons, x, y, quiet=FALSE) {
  # what to do if the user clicks with the mouse:
  # record click location
  clicked_pt <- list(
    x = graphics::grconvertX(x, from = "nic", to = "user"),
    y = graphics::grconvertY(y, from = "nic", to = "user")
  )
  # and print to screen
  if (!quiet){
  cat(paste(round(clicked_pt$x), ": ",
    round(clicked_pt$y, digits = 2), " degrees\n",
    sep = ""
  ))
  }
  NULL
}