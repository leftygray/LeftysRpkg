#' Add empty values into a vector
#'
#' This function is particularly useful function for placing tick marks between 
#' labels in ggplot objects
#' 
#' @details I found this function on Stack Overflow while looking for a way to 
#' add tick marks to ggplot plots. URL provided in references. Post with function 
#' code was created by user *adamdsmith*.  
#' 
#' @param x vector
#' @param nth specify which elements are removed, i.e., every nth
#' @param empty logical specifying where elements are dropped or replaced 
#' @param inverse logical specifying whether to drop every nth (FALSE; default)
#'   or save every nth (TRUE)
#'
#' @return Vector with every nth value removed or made into an empty string
#'
#' @references \url{https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r}
#'
#' @export
EveryNth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
