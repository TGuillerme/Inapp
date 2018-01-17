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
#' @author Thomas Guillerme <guillert@@tcd.ie>
#'
#' @references
#'
#' Brazeau, M. D., Guillerme, T., and Smith, M. R. 2017. Morphological 
#' phylogenetic analysis with inapplicable data. Biorxiv. <https://doi.org/10.1101/209775>
#'
#' @examples
#' \dontrun{
#' runInapp()
#' }
#' 
#' @exportPattern "^[[:alpha:]]+"
#' @import ape
#' @importFrom graphics plot legend
#' @importFrom grDevices pdf dev.off
#' @importFrom Rdpack reprompt 
#' @importFrom shiny runGitHub
#' @importFrom utils write.csv
NULL
