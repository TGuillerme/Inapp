#' @title Sates matrix
#'
#' @description Creates a states matrix
#'
#' @param tree \code{phylo}, a tree
#' @param character Either vector of character states (\code{"numeric"} or \code{"character"}) or a list of the same length of than the tips in the tree (see details)
#' @param inapplicable When the intended reconstruction method is \code{"Fitch"}, how do deal with inapplicable data: \code{1}, \code{2} for respectively treating them as ? or an extra state (default = \code{NULL}).
#' @param match.tip.char \code{logical}, \code{TRUE} to match the character to the tip labels (e.g character 1 matches with tip "a" or "1") or \code{FALSE} (default) to match the character to the tips entry (e.g. character 1 matches with the first tip)
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
#' -\code{$length}: a \code{numeric} vector recording the length of the tree.
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
    n_tip <- ape::Ntip(tree)
    n_node <- ape::Nnode(tree)
    
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
                one_char <- one_char[one_char != -1]
                one_char <- c(one_char, replace)
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
            replace <- max(unlist(character))+1
            character <- lapply(character, find.inapplicable, replace)
        }
    }

    ## Sorting the character to match the tip.labels entry
    if(match.tip.char == TRUE) {
        options(warn = -1)
        ## Check if tips are alphanumeric
        if(any(is.na(as.numeric(tree$tip.label)))) {
            ## Check if tips contain numeric
            if(all(grepl("\\d", tree$tip.label))) {
                ## Getting the numeric part of the tips in alphabetical order
                alpha_char <- unique(unlist(strsplit(tree$tip.label, split = "\\d")))
                ## Remove blanks
                alpha_char <- alpha_char[alpha_char != ""]
                ## Get the tips ordering
                tips_numbers <- unlist(strsplit(tree$tip.label, split = alpha_char))
                ## Remove blanks
                tips_numbers <- tips_numbers[tips_numbers != ""]
                ## Getting the tips order
                ordering <- match(tips_numbers, sort(tips_numbers))
            } else {
                ## Getting the tips in alphabetical order
                ordering <- match(tree$tip.label, sort(tree$tip.label))
            }
        } else {
            ## Getting the tips in numeric order
            ordering <- match(as.numeric(tree$tip.label), sort(as.numeric(tree$tip.label)))
        }
        options(warn = 0)
    } else {
        ordering <- seq_len(n_tip)
    }


    ## Add the character into the list
    states_matrix$Char[1:n_tip] <- character[ordering]

    ## Add tip labels

    ## Set up the active region tracker
    states_matrix$tracker <- list("Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)

    ## Set a applicable regions counts
    states_matrix$regions <- 0

    ## Save the node with changes
    states_matrix$changes <- numeric(0)

    ## Total length
    states_matrix$length <- numeric(0)

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
#' @param x A \code{dispRity} object.
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
        ## Length
        length <- x$regions + ifelse(length(x$changes) > 0, length(x$changes), 0)
        cat(paste("Tree length is:", length, "\n"))
        ## Details
        if(length != 0) {
            cat(paste(x$regions, "additional applicable regions.\n"))
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
        cat("No reconstructions calculated. See:\n ?apply.reconstruction\nto reconstruct ancestral states and count the tree length.\n")
    }
    return(invisible())
}



## Converts a character (inapplicable or missing)
convert.char <- function(character) {

    convert.inappli <- function(X) {
        return(ifelse(X == "-", -1, X))
    }

    convert.missing <- function(X, all_states) {
        if(X[1] == "?") {
            return(all_states)
        } else {
            return(X)
        }
    }

    ## Character is a list
    if(class(character) == "list") {
        if(unique(unlist(lapply(character, class))) != "numeric") {
            stop("Character list does not contain only numeric values.")
        } else {
            return(character)
        }
    }

    ## Character is a vector (numeric)
    if(class(character) == "numeric") {
        return(as.list(character))
    }
    
    ## Character is not numeric
    if(class(character) == "character") {
    
        if(length(character) == 1) {
            #Split the character chain
            character <- as.character(strsplit(as.character(character), "")[[1]])
            ## Check for polymorphic characters
            polymorphic_start <- which(character == "{")
            polymorphic_end <- which(character == "}")
            if(length(polymorphic_start) != length(polymorphic_end)) {
                stop("Some brackets are missing for polymorphic characters")
            }

            ## Dealing with the polymorphic positions
            if(length(polymorphic_start) > 0) {
                poly_states <- list()
                for(one_char in 1:length(polymorphic_start)) {
                    ## Getting the character states
                    poly_states[[one_char]] <- character[(polymorphic_start[one_char]+1):(polymorphic_end[one_char]-1)]
                    ## Removing the polymorphy
                    character[(polymorphic_start[one_char]+1):polymorphic_end[one_char]] <- NA
                }
                ## Remove NAs (the polymorphies)
                character <- character[!is.na(character)]
            }

        } else {
            polymorphic_start <- numeric()
        }

        ## Convert into list
        character <- as.list(character)
        if(length(polymorphic_start) > 0) {
            ## add the polymorphic characters back in position
            poly_position <- which(character == "{")
            ## Replace the characters
            for(one_char in 1:length(polymorphic_start)) {
                character[[poly_position[one_char]]] <- poly_states[[one_char]]
            }
        }

        ## Convert inapplicable
        character <- lapply(character, convert.inappli)
        
        ## Get all states
        options(warn = -1)
        all_states <- unlist(lapply(character, as.numeric))
        options(warn = 0)
        all_states <- unique(all_states[!is.na(all_states)]) # | (all_states != -1))]

        ## Convert missing
        character <- lapply(character, convert.missing, all_states)

        ## Convert into numeric
        return(lapply(character, function(X) sort(as.numeric(X))))
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