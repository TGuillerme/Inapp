#' @title Read any tree
#'
#' @description Reads any type of tree, whether it's a nexus or newick one.
#'
#' @param file a file name specified by either a variable of mode character, or a double-quoted string.
#' @param ... any optional arguments to be passed to \code{\link[ape]{read.nexus}} or \code{\link[ape]{read.tree}}.
#' 
#' @examples
#' ## Generating a single newick file
#' newick <- "(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"
#' cat(newick, file = "example_newick.tre", sep = "\n")
#' ## Reading a single newick file
#' read.any.tree("example_newick.tre")
#' ## Removing the file
#' unlink("example_newick.tre")
#' 
#' ## Generating a multiple nexus file
#' ape::write.nexus(ape::rmtree(2,3), file = "example_multi_nexus.nex")
#' ## Reading a multiple nexus file
#' read.any.tree("example_multi_nexus.nex")
#' ## Removing the file
#' unlink("example_multi_nexus.nex") 
#'
#' @author Thomas Guillerme
#' @export

read.any.tree <- function(file, ...) {

    if(scan(what = "#NEXUS", file = file, nlines = 1, quiet = TRUE) == "#NEXUS") {
        ## The tree is a true nexus
        return(ape::read.nexus(file, ...))
    } else {
        ## The tree is a true newick
        return(ape::read.tree(file, ...))
    }
}