#' Vignette Plot
#'
#' Creates a plot formatted for inclusion in vignettes
#'
#' @param tree Tree to plot
#' @param character \code{character} string listing character states to plot;
#'                  tokens will be matched to labels in alphabetical order.
#' @param na \code{logical}, should `-` be treated as `inapplicable`?
#' @param legend.pos \code{character} Position to plot legend.
#' @param counts \code{integer} vector specifying which counts to show: see [plot.states.matrix] for details
#' @param passes \code{integer} specifying which passes to plot; specify 0 for none.
#' @param state.labels Will be passed directly to \code{\link{plot.states.matrix}}
#' @param state.override \code{list} specifying states to reconstruct at each tip
#'                       and node, instead of those reconstructed
#' @param score.override,changes.override,regions.override \code{integer}s to
#' override the values calculated
#' @param \dots Additional parameters to pass to \code{\link{plot.states.matrix}}.
#'
#' @author Martin R. Smith
#' @keywords Internal
#'
#' @importFrom graphics par
#' @importFrom grDevices dev.new dev.off
#' @export
#' 

vignettePlot <- function (tree, character, na=TRUE, legend.pos='bottomleft',
                          passes=if(na) 1:4 else 1:2, state.labels = character(0),
                          counts=if(na) 1:2 else 2, state.override = NULL,
                          score.override = NULL, changes.override = NULL,
                          regions.override = NULL, ...) {
  tree$edge.length <- rep(1, dim(tree$edge)[1])
  reconstruction <- apply.reconstruction(tree, character,
                                         method=if(na) "NA" else 'Fitch',
                                         match.tip.char=TRUE)
  if (!is.null(state.override)) reconstruction$Up2 <- state.override
  if (!is.null(score.override)) reconstruction$score <- score.override
  if (!is.null(changes.override)) reconstruction$changes <- changes.override
  if (!is.null(regions.override)) reconstruction$regions <- regions.override
  dev.new(); plot(tree, direction='upwards', ...); corners <- par('usr'); dev.off()
  plot.states.matrix(reconstruction,
       passes=passes, counts=counts,
       direction='upwards', legend.pos=legend.pos, state.labels=state.labels,
       col.states=TRUE, use.edge.length=TRUE,
       x.lim=c(-3, corners[2]), y.lim=c(-1, corners[4]+0.1), ...)
}
