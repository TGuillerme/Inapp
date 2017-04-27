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
        n_passes <- list(first.downpass, first.uppass, second.downpass, second.uppass)
    } else {
        n_passes <- list(fitch.downpass, fitch.uppass)
    }

    ## Applying the passes for each node
    for (pass in 1:passes) {
        states_matrix <- n_passes[[pass]](states_matrix)
    }

    return(states_matrix)
}
