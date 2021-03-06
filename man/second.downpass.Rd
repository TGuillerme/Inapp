% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NA.reconstruction.R
\name{second.downpass}
\alias{second.downpass}
\title{Second downpass}
\usage{
second.downpass(states_matrix)
}
\arguments{
\item{states_matrix}{A \code{list} contains all the states and the activations}
}
\description{
Applies a second down pass to a node
}
\examples{
## Create a states matrix for reconstruction
tree <- ape::read.tree(text = "((a,b),(c,d));")
NA_matrix <- make.states.matrix(tree, "01?-")

## Apply the second downpass
(NA_matrix <- second.downpass(first.uppass(first.downpass(NA_matrix))))

## Access the states
NA_matrix$Dp2

}
\seealso{
\code{\link{apply.reconstruction}}
}
\author{
Thomas Guillerme
}
