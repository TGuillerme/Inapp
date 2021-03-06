% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svg.R
\name{SVGTree}
\alias{SVGTree}
\title{SVG Tree}
\usage{
SVGTree(
  treeNo,
  canvas,
  char,
  stateLabels,
  treeNames,
  charIndex = 0,
  analysisLabels = character(0)
)
}
\arguments{
\item{treeNo}{Integer specifying which of the trees on the SVGForest to plot}

\item{canvas}{an object of class SVGForest.}

\item{char}{Character string specifying character to optimise on the tree}

\item{stateLabels}{Character vector specifying label for each applicable state
of the character}

\item{treeNames}{Character string naming the analysis used to generate each tree}

\item{charIndex}{Character string specifying the number of the character,
for inclusion under the SVG element's \code{data-char} attribute.}

\item{analysisLabels}{Character vector specifying label for each analysis}
}
\value{
Character string describing an SVG object that depicts the tree,
which can be written to file or included in markdown destined for HTML
output.
}
\description{
Plots one of the character reconstructions embodied within an \link{SVGCanvas}
object on a specified phylogenetic tree, returning SVG text that can be
saved to file or included directly in an HTML document.
}
\author{
Martin R. Smith
}
