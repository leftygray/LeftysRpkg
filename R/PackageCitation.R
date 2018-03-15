## PackageCitations.R

# This personal function is used to list all the packages loaded during a 
# session and provide their citation. 

# Richard T. Gray (based on http://www.carlboettiger.info/2012/03/20/citing-r-packages.html)

PackageCitation <- function(packages, bibtex = FALSE, file = NULL){ 
  # This function outputs the package citation information of a package
  # or all packages loaded in the current session. 
  
  # Args:
  #   packages: Vector of package names we want to cite. If "all" display 
  #     all packages loaded. 
  #   bibtex: If TRUE display bibtex citation only. 
  #     
  #   Returns:
  #     Returns a list of the citation information and outputs a bibtex 
  #       file if requested.  
  #       
  # -----------------------------------------------------------------------
  
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
