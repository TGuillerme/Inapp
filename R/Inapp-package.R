#' Inapplicable Discrete Characters Reconstruction
#'
#' An algorithm allowing states reconstruction and tree score calculation for inapplicable discrete characters
#'
#' @name Inapp-package
#'
#' @aliases Inapp
#'
#' @docType package
#'
#' @author Thomas Guillerme <guillert@@tcd.ie> & Martin D. Brazeau & Martin R. Smith.
#'
#' @references
#' 
#' Brazeau, M. D., Guillerme, T., and Smith, M. R. (2018). An algorithm for Morphological
#' Phylogenetic Analysis with Inapplicable Data. Systematic Biology.
#' <https://dx.doi.org/10.1093/sysbio/syy083>
#'
#' @keywords maximum parsimony, inapplicable characters, states reconstruction, tree score
#'
#' @examples
#' \dontrun{
#' runInapp()
#' }
#'
#' @exportPattern "^[[:alpha:]]+"
#' @import ape
#' @importFrom graphics plot legend
#' @importFrom shiny runGitHub runApp updateNumericInput
#' @importFrom grDevices dev.off pdf rainbow
#' @importFrom utils write.csv
NULL
