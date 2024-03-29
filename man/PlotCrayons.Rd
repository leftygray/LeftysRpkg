% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotCrayons.R
\name{PlotCrayons}
\alias{PlotCrayons}
\title{Illustration of crayon colors which is a copy of Karl Broman's function}
\usage{
PlotCrayons(method2order = c("hsv", "cluster"), cex = 0.6, mar = rep(0.1, 4))
}
\arguments{
\item{method2order}{Method to order colors (\code{"hsv"} or \code{"cluster"})}

\item{cex}{Character expansion for the text}

\item{mar}{Margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})}
}
\value{
None. Produces a plot.
}
\description{
Copyright (C) 2018 Karl Broman
GNU General Public License, version 3, available at https://www.r-project.org/Licenses/GPL-3
}
\details{
This function Creates a plot of the crayon colors in \code{\link{PlotColors}}
}
\examples{
plot_crayons()

}
\references{
\url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}

\url{https://github.com/kbroman/pkg_primer/blob/gh-pages/example/stage5/R/Plot_crayons.R}
}
\seealso{
\code{\link{PlotColors}}
}
\author{
Karl W Broman, \email{kbroman@biostat.wisc.edu}
}
\keyword{hplot}
