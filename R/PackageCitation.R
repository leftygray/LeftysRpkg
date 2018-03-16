#' A function to get the citations of packages
#'
#' This function outputs the package citation information of a package
#' or all packages loaded in the current session. It is based on a post by 
#' Carl Boettinger referenced below. 
#'
#' @param packages Vector of package names we want to cite. If "all" display all
#' packages loaded.
#' @param bibtex If TRUE display bibtex citation only.
#' @param file If specified is a string giving the file path
#' and name.
#'
#' @return Returns a list of the citation information and outputs a bibtex file
#' if requested.
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' @references \url{http://www.carlboettiger.info/2012/03/20/citing-r-packages.html}
#'
#' @examples
#' PackageCitation("all")
#'
#' @export
#'
PackageCitation <- function(packages, bibtex = FALSE, file = NULL){

  # Generate list of package citations
  if (length(packages) == 1) {
    if (packages != "all") {
      output <- ifelse(bibtex, toBibtex(citation(packages)),
                       citation(packages))
    } else {
      if (bibtex) {
        output <- sapply(names(sessionInfo()$otherPkgs),
                       function(x) toBibtex(citation(x)))
      } else {
        output <- sapply(names(sessionInfo()$otherPkgs),
                         function(x) citation(x))
      }
    }
  } else {
    if (bibtex) {
      output <- lapply(packages, function(x) toBibtex(citation(x)))
    } else {
      output <- lapply(packages, function(x) citation(x))
    }
    names(output) <- packages
  }

  # Save output time file
  if (!is.null(file)) {
    sink(file)
    print(output)
    sink()
  }

  return(output)
}
