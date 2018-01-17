#' @title Apply reconstruction
#'
#' @description Apply a series of passes for an ancestral states reconstruction
#'
#' @param tree \code{phylo}, a tree
#' @param character \code{character}, a vector of character states
#' @param passes \code{numeric}, the number of passes in the tree; from \code{1} to \code{4} (default)
#' @param method either \code{"Fitch"} or \code{"NA"}
#' @param inapplicable When method is \code{"Fitch"}, how do deal with inapplicable data: \code{1}, \code{2} for respectively treating them as ? or an extra state.
#' @param match.tip.char \code{logical}, \code{TRUE} to match the character to the tip labels (e.g character 1 matches with tip "a" or "1") or \code{FALSE} (default) to match the character to the tips entry (e.g. character 1 matches with the first tip)
#' 
#' @return
#' A \code{states.matrix} object that is a \code{list} of:
#'  \item{\code{$Char}}{a \code{list} of character (\code{numeric}) where "NA" is equal to \code{-1}}
#'  \item{\code{$Dp1}}{a \code{list} of character from the first downpass}
#'  \item{\code{$Up1}}{a \code{list} of character from the first uppass}
#'  \item{\code{$Dp2}}{a \code{list} of character from the second downpass}
#'  \item{\code{$Up2}}{a \code{list} of character from the second uppass}
#'  \item{\code{$tracker}}{a \code{list} of the same \code{Dp1}, \code{Up1}, \code{Dp2} and \code{Up2} elements but tracking the applicable regions}
#'  \item{\code{$region}}{a vector of \code{integer} value indicating the nodes where an additional applicable region was recorded}
#'  \item{\code{$changes}}{a vector of \code{itneger} values indicating the nodes where a state change was recorded}
#'  \item{\code{$score}}{a single \code{integer} value that is the score of the tree (\code{X$score = length(X$region) + length(X$changes)})}
#'  \item{\code{$tree}}{the tree (\code{phylo})}
#' 
#' @examples
#' set.seed(1)
#' ## Random tree with 12 taxa
#' tree <- ape::rtree(12, br = NULL)
#' ## A character with inapplicable data
#' character <- "23--1??--032"
#' 
#' ## Normal Fitch algorithm (NA states are missing data)
#' apply.reconstruction(tree, character, passes = 2, method = "Fitch",
#'                      inapplicable = 1)
#' ## Same but NA states are an extra state and character now match the tips
#' apply.reconstruction(tree, character, passes = 2, method = "Fitch",
#'                      inapplicable = 2, match.tip.char = TRUE)
#' 
#' ## NA algorithm
#' apply.reconstruction(tree, character, passes = 4, method = "NA")
#' 
#' ## 1st pass of the NA algorithm
#' apply.reconstruction(tree, character, passes = 1, method = "NA")
#' 
#' @seealso \code{\link{plot.states.matrix}}, \code{\link{runInapp}}
#' 
#' @author Thomas Guillerme
#' @export

apply.reconstruction <- function(tree, character, passes = 4, method, inapplicable, match.tip.char = FALSE) {

    ## Method
    if(!(method %in% c("NA","Fitch"))) {
        stop("method should be 'Fitch' or 'NA'")
    }

    ## Set up the Inapplicable interpretation
    if(method == "NA") {
        inapplicable = NULL
    } else {
        if(missing(inapplicable)) {
            ## Treating as missing by default
            inapplicable = 1
        } else {
            if(!(inapplicable %in% c(1,2))) {
                stop("Inapplicable argument should be 1 (treated as ?) or 2 (treated as an extra state).")
            }
        }
    }

    ## Setting up the output state matrix
    states_matrix <- make.states.matrix(tree, character, inapplicable, match.tip.char)

    ## Setting the list of passes
    if(method == "NA") {
        passes <- list(first.downpass, first.uppass, second.downpass, second.uppass)
    } else {
        passes <- list(fitch.downpass, fitch.uppass)
    }

    ## Applying the passes for each node
    for (pass in passes) {
        states_matrix <- pass(states_matrix)
    }

    ## Get the total length of the tree
    states_matrix$length <- score.from(states_matrix$changes) + score.from(states_matrix$regions)

    return(states_matrix)
}
