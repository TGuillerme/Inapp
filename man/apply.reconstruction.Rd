% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply.reconstruction.R
\name{apply.reconstruction}
\alias{apply.reconstruction}
\title{Apply reconstruction}
\usage{
apply.reconstruction(
  tree,
  character,
  passes = 4,
  method = "NA",
  inapplicable = 1,
  match.tip.char = FALSE
)
}
\arguments{
\item{tree}{\code{phylo}, a tree}

\item{character}{\code{character}, a vector of character states}

\item{passes}{\code{numeric}, the number of passes in the tree; from \code{1} to \code{4} (default)}

\item{method}{either \code{"Fitch"} or \code{"NA"}}

\item{inapplicable}{When method is \code{"Fitch"}, how do deal with inapplicable data: \code{1}, \code{2} for respectively treating them as ? or an extra state.}

\item{match.tip.char}{\code{logical}, \code{TRUE} to match the character to the tip labels (e.g character 1 matches with tip "a" or "1") or \code{FALSE} (default) to match the character to the tips entry (e.g. character 1 matches with the first tip)}
}
\value{
A \code{states.matrix} object that is a \code{list} of:
\item{\code{$Char}}{a \code{list} of character (\code{numeric}) where "NA" is equal to \code{-1}}
\item{\code{$Dp1}}{a \code{list} of character from the first downpass}
\item{\code{$Up1}}{a \code{list} of character from the first uppass}
\item{\code{$Dp2}}{a \code{list} of character from the second downpass}
\item{\code{$Up2}}{a \code{list} of character from the second uppass}
\item{\code{$tracker}}{a \code{list} of the same \code{Dp1}, \code{Up1}, \code{Dp2} and \code{Up2} elements but tracking the applicable regions}
\item{\code{$region}}{a vector of \code{numeric} value indicating the number of additional applicable regions}
\item{\code{$changes}}{a vector of \code{numeric} values indicating the nodes where a state change was recorded}
\item{\code{$score}}{a single \code{numeric} value that is the score of the tree (\code{X$score = length(X$region) + length(X$changes)})}
\item{\code{$tree}}{the tree (\code{phylo})}
}
\description{
Apply a series of passes for an ancestral states reconstruction
}
\examples{
set.seed(1)
## Random tree with 12 taxa
tree <- ape::rtree(12, br = NULL)
## A character with inapplicable data
character <- "23--1??--032"

## Normal Fitch algorithm (NA states are missing data)
apply.reconstruction(tree, character, passes = 2, method = "Fitch",
                     inapplicable = 1)
## Same but NA states are an extra state and character now match the tips
apply.reconstruction(tree, character, passes = 2, method = "Fitch",
                     inapplicable = 2, match.tip.char = TRUE)

## NA algorithm
apply.reconstruction(tree, character, passes = 4, method = "NA")

## 1st pass of the NA algorithm
apply.reconstruction(tree, character, passes = 1, method = "NA")

}
\seealso{
\code{\link{plot.states.matrix}}, \code{\link{runInapp}}
}
\author{
Thomas Guillerme
}
