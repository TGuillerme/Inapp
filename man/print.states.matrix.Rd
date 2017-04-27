% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NA.utilities.R
\name{print.states.matrix}
\alias{print.states.matrix}
\title{Prints a \code{states.matrix} object.}
\usage{
\method{print}{states.matrix}(x, ...)
}
\arguments{
\item{x}{A \code{dispRity} object.}

\item{...}{further arguments to be passed to \code{print}.}
}
\description{
Summarises the content of a \code{states.matrix} object.
}
\examples{
## A simple topology
tree <- read.tree(text = "((a,b),(c,d));")

## A simple character
character <- "01?-"

## Create a states matrix for reconstruction
make.states.matrix(tree, character)


}
\author{
Thomas Guillerme
}
\seealso{
\code{\link{make.states.matrix}}.
}
