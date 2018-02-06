#' Vignette Plot
#'
#' Creates a plot formatted for inclusion in vignettes
#'
#' @param tree Tree to plot
#' @param character \code{character} string listing character states to plot;
#'                  tokens will be matched to labels in alphabetical order
#' @param na \code{logical}, should `-` be treated as `inapplicable`?
#' @param legend.pos \code{character} Position to plot legend.
#' @param \dots Additional parameters to pass to \code{\link{plot.states.matrix}}
#'
#' @author Martin R. Smith
#' @keywords Internal
#'
#' @export
vignettePlot <- function (tree, character, na=TRUE, legend.pos='bottomleft', ...) {
  tree$edge.length <- rep(1, dim(tree$edge)[1])
  reconstruction <- apply.reconstruction(tree, character,
                                         method=if(na) "NA" else 'Fitch',
                                         match.tip.char=TRUE)
  dev.new(); plot(tree, direction='upwards', ...); corners <- par('usr'); dev.off()
  plot(reconstruction,
       passes=if(na) 1:4 else 1:2, counts=if(na) 1:2 else 2,
       direction='upwards', legend.pos=legend.pos,
       col.states=TRUE, use.edge.length=TRUE,
       x.lim=c(-3, corners[2]), y.lim=c(-1, corners[4]+0.1), ...)
}
