#' @title Sates matrix
#'
#' @description Creates a states matrix
#'
#' @param tree \code{phylo}, a tree
#' @param character Either vector of character states (\code{"numeric"} or \code{"character"}) or a list of the same length of than the tips in the tree (see details)
#' @param inapplicable When the intended reconstruction method is \code{"Fitch"}, how do deal with inapplicable data: \code{1}, \code{2} for respectively treating them as ? or an extra state (default = \code{NULL}).
#' @param match.tip.char \code{logical}, \code{TRUE} to match the character
#' to the tip labels (e.g character 1 matches with tip "a" or "1")
#' or \code{FALSE} (default) to match the character to the tips entry
#'  (e.g. character 1 matches with the first tip).  If a character is supplied
#'  as a named list, and the names correspond to the names of the tips, then
#'  this parameter will be ignored and the names of the characters will be used
#'  to match them to the names of the tips.
#'
#' @details
#' If \code{character} argument is a list, each element of the list must be a \code{"numeric"} vector with \code{"?"} being all states and \code{"-"} being \code{-1}.
#'
#' @return
#' A list of character states for each node and tip per pass:
#' -\code{$Char}: a \code{list} of character states for the tips.
#' -\code{$Dp1}: a \code{list} of the tips' and nodes' states after the first downpass.
#' -\code{$Up1}: a \code{list} of the tips' and nodes' states after the first uppass.
#' -\code{$Dp2}: a \code{list} of the tips' and nodes' states after the second downpass.
#' -\code{$Up2}: a \code{list} of the tips' and nodes' states after the second uppass.
#' -\code{$tracker}: a \code{list} tracking the applicable regions.
#' -\code{$regions}: a single \code{numeric} value counting the number of applicable regions.
#' -\code{$changes}: a \code{numeric} vector recording the node with state changes.
#' -\code{$score}: a \code{numeric} vector recording the score of the tree.
#' -\code{$tree}: a \code{phylo} object describing the tree.
#' -\code{$n_tip}: a \code{numeric} vector recording the number of tips.
#' -\code{$n_node}: a \code{numeric} vector recording the number of (internal) nodes within the tree.
#'
#' @examples
#' ## A simple topology
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#'
#' ## A simple character
#' character <- "0101"
#'
#' ## Create a states matrix for reconstruction
#' make.states.matrix(tree, character)
#'
#' ## A complex character
#' character <- "0{01}-?"
#'
#' @seealso \code{\link{apply.reconstruction}}.
#'
#' @author Thomas Guillerme
#' @export

make.states.matrix <- function(tree, character, inapplicable = NULL, match.tip.char = FALSE) {

    ## Check if the tree is a tree!
    if(class(tree) != "phylo") {
        stop("The tree must be of class 'phylo'.")
    }

    # Read tree properties
    tip_labels <- tree$tip.label
    n_tip <- length(tip_labels)
    n_node <- tree$Nnode

    ## Transform character
    if(class(character) != "list") {
        character <- convert.char(character)
    }

    ## Check if the character is the same length as the tree
    if(n_tip != length(character)) {
        stop("The tree and character arguments don't match.")
    }

    ## Set up the list of characters
    filling <- vector("list", n_tip + n_node)
    states_matrix <- list("Char" = filling, "Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)

    if(!is.null(inapplicable)) {
        ## How to treat inapplicables (for Fitch)
        find.inapplicable <- function(one_char, replace) {
            if(any(one_char == -1)) {
                one_char <- c(one_char[one_char != -1], replace)
                one_char <- sort(unique(one_char))
            }
            return(one_char)
        }
        if(inapplicable == 1) {
            ## Inapplicable are missing
            unique_characters <- unique(unlist(character))
            replace <- unique_characters[unique_characters != -1]
            character <- lapply(character, find.inapplicable, replace)
        } else {
            ## Inapplicable is an extra state
            replace <- max(unlist(character)) + 1
            character <- lapply(character, find.inapplicable, replace)
        }
    }

    ## Sorting the character to match the tip.labels entry
    if (all(tip_labels %in% names(character))) {
        ordering <- match(tip_labels, names(character))
        
    } else {
        if (match.tip.char == TRUE) {
            ## Check if tips are alphanumeric
            if(all(vapply(tip_labels, is.numeric, logical(1)))) {
              ## Get the tips in numeric order
              ordering <- match(as.numeric(tip_labels), sort(as.numeric(tip_labels)))
            } else {
              matches <- gregexpr("\\d+(\\.\\d+)?", tip_labels, perl=TRUE)
              if (all(matches > -1)) {
                  ## All tips contain numeric
                  tip_numbers <- regmatches(tip_labels, matches)
                  if (any (vapply(tip_numbers, length, integer(1)) > 1)) {
                    stop("Some tips bear multiple numeric components.")
                  }
                  tip_numbers <- as.numeric(tip_numbers)
                  ordering <- match(tip_numbers, sort(tip_numbers))
                } else {
                    ## Getting the tips in alphabetical order
                    ordering <- match(tip_labels, sort(tip_labels))
                }
            }
        } else {
            # i.e. match.tip.char == FALSE
            ordering <- seq_len(n_tip)
        }
    }

    ## Add the character into the list
    states_matrix$Char[1:n_tip] <- character[ordering]

    ## Add tip labels

    ## Set up the active region tracker
    states_matrix$tracker <- list("Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)

    ## Set a applicable regions counts
    states_matrix$regions <- integer(0)

    ## Save the node with changes
    states_matrix$changes <- integer(0)

    ## Total score
    states_matrix$score <- integer(0)

    ## Add the tree, with its properties
    states_matrix$tree <- tree
    states_matrix$n_tip <- n_tip
    states_matrix$n_node <- n_node


    ## Set up the NA_matrix class
    class(states_matrix) <- "states.matrix"

    return(states_matrix)
}


#' @title Prints a \code{states.matrix} object.
#'
#' @description Summarises the content of a \code{states.matrix} object.
#'
#' @param x A \code{states.matrix} object.
#' @param ... further arguments to be passed to \code{print}.
#'
#' @examples
#' ## A simple topology
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#'
#' ## A simple character
#' character <- "01?-"
#'
#' ## Create a states matrix for reconstruction
#' make.states.matrix(tree, character)
#'
#'
#' @seealso \code{\link{make.states.matrix}}.
#'
#' @author Thomas Guillerme
#' @export
#'

print.states.matrix <- function(x, ...) {

    ## Record the call
    match_call <- match.call()
    x_name <- ifelse(class(match_call$x) == "name", as.character(as.name(match_call$x)), "states_matrix")

    ## Number of tips
    num_tips <- ceiling(length(x$Char)/2)

    ## States
    all_states <- sort(as.character(unique(unlist(x$Char))))
    all_states <- gsub("-1", "-", all_states)

    ## Passes done
    passes <- vector()
    for(pass in 2:5) {
        passes[pass-1] <- ifelse(is.null(unlist(x[[pass]])), FALSE, TRUE)
    }
    pass_names <- c("1st Downpass", "1st Uppass", "2nd Downpass", "2nd Uppass")
    pass_ID <- c("$Dp1", "$Up1", "$Dp2", "$Up2")


    ## Printing the object
    cat(" ---- Tree ---- \n")
    x$tree$edge.length <- NULL
    cat(paste(ape::write.tree(x$tree), "\n"))

    cat(" ---- States matrix---- \n")
    ## Tips and states
    cat(paste("Number of tips =", num_tips, "\n"))
    cat(paste("Character states =", paste(all_states, collapse = ", "), "\n"))
    if(any(passes)) {
        ## Passes completed
        for(pass in 1:4) {
            cat(paste(pass_names[pass]))
            if(passes[[pass]]) {
                cat(paste(" completed. (See element ", pass_ID[pass], ").\n", sep = ""))
            } else {
                cat(": NULL\n")
            }
        }
        ## Score
        score <- length(x$regions) + ifelse(length(x$changes) > 0, length(x$changes), 0)
        cat(paste("Character adds", score, "to tree score\n"))
        ## Details
        if(score != 0) {
            cat(paste(length(x$regions), "additional applicable regions.\n"))
            if(length(x$changes) > 1) {
                cat("State changes at nodes: ", paste(x$changes, collapse = ", "), ".\n", sep = "")
            } else {
                if(length(x$changes) == 1) {
                    cat("State changes at node: ", x$changes, ".\n", sep = "")
                } else {
                    cat("No state changes.\n")
                }
            }
        }
    } else {
        cat("No reconstructions calculated. See:\n ?apply.reconstruction\nto reconstruct ancestral states and score the tree.\n")
    }
    return(invisible())
}



#' Convert character
#'
#' Converts a character (inapplicable or missing)
#' @param character List, numeric, character or matrix describing a morphological character
#'
#' @return The character as a list, with each entry relating the state of the character
#'         for the corresponding taxon
#'
#' @export
#' @keywords internal
convert.char <- function(character) {

    ## Character is a list
    if (class(character) == "list") {
        if(unique(unlist(lapply(character, class))) != "numeric") {
            stop("Character list does not contain only numeric values.")
        } else {
            return(character)
        }
    }

    ## Character is a vector (numeric)
    if (class(character) == "numeric") {
        return(as.list(character))
    }

    ## Character is a matrix (not necessarily numeric)
    if (class(character) == 'matrix') {
        character <- character[, 1]
    }

    ## Character is not numeric
    if(class(character) == "character") {
        character <- gsub(',', '', character, fixed=TRUE)

        ## Get all the states
        stateslist <- paste0(character, collapse='')
        matches <- gregexpr("[0-9\\-]", stateslist, perl=TRUE)
        all_states <- unique(regmatches(stateslist, matches)[[1]])

        ## Split into individual character states
        if(length(character) == 1) {
            matches <- gregexpr("\\{[^\\}]+\\}|.", character, perl=TRUE)
            character <- regmatches(character, matches)[[1]]
        }
        character <- as.list(character)

        ## Convert ambiguous
        character[character=="?"] <- list(all_states)

        ## Convert inapplicable
        character <- lapply(character, function (states) {
          states <- unlist(strsplit(gsub("[\\{\\}\\(\\)]", "", states), ''))
          states[states=='-'] <- -1
          sort(as.numeric(states))
        })
        return(character)
    }
}

## Selects descendant and ancestor
desc.anc <- function (node, tree) {
    tree.edge <- tree$edge
    parent <- tree.edge[, 1]
    child  <- tree.edge[, 2]
    descendants <- child[parent == node]
    ancestor <- parent[child == node]
    return(c(descendants, ancestor))
}

## Get an union (&)
get.common <- function(a, b) {
    if(length(a) >= length(b)) {
        out <- sort(a[a %in% b])
    } else {
        out <- sort(b[b %in% a])
    }

    if(length(out) == 0) {
        out <- NULL
    }
    return(unique(out))
}

## Get an intersection inclusive (|)
get.union.incl <- function(a, b) {
    out <- unique(c(a,b))
    if(length(out) == 0) {
        return(NULL)
    } else {
        return(sort(unique(out)))
    }
}

## Get an intersection exclusive (^)
get.union.excl <- function(a, b) {
    out <- get.union.incl(a,b)
    out <- out[!(out %in% get.common(a,b))]
    if(length(out) == 0) {
        return(NULL)
    } else {
        return(sort(unique(out)))
    }
}


# Score from marked nodes
#
# @param counted.nodes An \code{integer} vector listing nodes at which a score was noted
#
# @return The number of items in the vector: zero if none
# @keywords internal
score.from <- function (counted.nodes) {
  if (length(counted.nodes)) return (length(counted.nodes)) else return (0);
}
