#' Select crayon colors
#'
#' Vector of colors selected from Crayola crayons. It is essentially a copy 
#' (with some minor tweaks) from Karl Broman's brocolors.R function:
#' https://github.com/kbroman/pkg_primer/blob/gh-pages/example/stage5/R/brocolors.R
#'
#' @param color_names Optional vector of color names; can be partial matches.
#' @param ... Additional optional color names
#'
#' @return Vector of named RGB colors
#'
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @references \url{https://github.com/kbroman/broman/blob/master/R/brocolors.R}
#' @references  \url{http://kbroman.org/blog/2017/05/02/crayon-colors-simplified/}
#'
#' @seealso \code{\link{PlotCrayons}}, \code{\link{PlotColors}}
#' 
#' @examples
#' SelectCrayons("purple m")
#' SelectCrayons(c("purple m", "tickle"))
#' SelectCrayons("purple m", "tickle")
#' 
#' @export
#' @keywords utilities
#' 
SelectCrayons <- function(color_names=NULL, ...) {
    
    # Get crayon colours
    crayons <- PlotColors("crayons")
    
    # Return default
    if(is.null(color_names)) return(crayons)

    # Started extracting specific colors
    dots <- list(...)
    color_names <- unlist(c(color_names,dots))

    allnames <- names(crayons)

    # Look for exact matches
    m <- match(color_names, allnames)

    # Items not found: do grep. Require exactly one match
    notfound <- color_names[is.na(m)]
    g <- vapply(notfound, function(a) {
        z <- grep(a, allnames, ignore.case=TRUE)
        if(length(z) < 1) return(-1) # not found
        if(length(z) > 1) return(-2) # found multiply
        z }, 1)

    # Issue warning if some not found or some found multiply
    if(any(g < 0)) {
        if(any(g == -1)) warning("Some colors not found")
        if(any(g == -2)) warning("Some colors with multiple matches")
    }
    
    g[g < 0] <- NA
    m[is.na(m)] <- g
    
    # Final result
    result <- crayons[g]
    
    # For those not found singly, add input as names
    names(result)[is.na(g)] <- color_names[is.na(g)]
    
    # Return result
    return(result)
}
