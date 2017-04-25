#' @title Sates matrix
#'
#' @description Creates a states matrix
#'
#' @param tree \code{phylo}, a tree
#' @param character Either vector of character states (\code{"numeric"} or \code{"character"}) or a list of the same length of than the tips in the tree (see details)
#' @param match.tip.char \code{logical}, \code{TRUE} to match the character to the tip labels (e.g character 1 matches with tip "a" or "1") or \code{FALSE} (default) to match the character to the tips entry (e.g. character 1 matches with the first tip)
#'
#' @details
#' If \code{character} argument is a list, each element of the list must be a \code{"numeric"} vector with \code{"?"} being all states and \code{"-"} being \code{-1}.
#' @author Thomas Guillerme
#' @export

make.states.matrix <- function(tree, character, inapplicable = NULL, match.tip.char = FALSE) {

    ## Check if the tree is a tree!
    if(class(tree) != "phylo") {
        stop("The tree must be of class 'phylo'.")
    }

    ## Transform character
    if(class(character) != "list") {
        character <- convert.char(character)
    }

    ## Check if the character is the same length as the tree
    if(ape::Ntip(tree) != length(character)) {
        stop("The tree and character arguments don't match.")
    }

    ## Set up the list of characters
    filling <- vector("list", ape::Ntip(tree)+ape::Nnode(tree))
    states_matrix <- list("Char" = filling, "Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)


    if(!is.null(inapplicable)) {
        ## How to treat inapplicables (for Fitch)
        find.inapplicable <- function(one_char, replace) {
            if(any(one_char == -1)) {
                one_char <- one_char[which(one_char != -1)]
                one_char <- c(one_char, replace)
                one_char <- sort(unique(one_char))
            }
            return(one_char)
        }
        if(inapplicable == 1) {
            ## Inapplicable are missing
            replace <- unique(unlist(character))[which(unique(unlist(character)) != -1)]
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
                alpha_char <- alpha_char[which(alpha_char != "")]
                ## Get the tips ordering
                tips_numbers <- unlist(strsplit(tree$tip.label, split = alpha_char))
                ## Remove blanks
                tips_numbers <- tips_numbers[which(tips_numbers != "")]
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
        ordering <- seq(1:ape::Ntip(tree))
    }


    ## Add the character into the list
    states_matrix$Char[1:ape::Ntip(tree)] <- character[ordering]

    ## Set up the active region tracker
    states_matrix$tracker <- list("Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)

    ## Set a applicable regions counts
    states_matrix$regions <- 0

    ## Save the node with changes
    states_matrix$changes <- numeric(0)

    return(states_matrix)
}

# ' @title Convert character
# '
# ' @description Convert a character if it is not numeric (transforming - into -1 and ? into all characters (but - ))
# '
# ' @param character any character vector
# '
# ' @author Thomas Guillerme

convert.char <- function(character) {

    convert.inappli <- function(X) {
        return(ifelse(X == "-", -1, X))
    }

    convert.missing <- function(X, all_states) {
        if(X == "?") {
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
        } 
        ## Convert into list
        character <- as.list(character)

        ## Convert inapplicable
        character <- lapply(character, convert.inappli)
        
        ## Get all states
        options(warn = -1)
        all_states <- as.numeric(character)
        options(warn = 0)
        all_states <- unique(all_states[-c(which(is.na(all_states)))]) #, which(all_states == -1))]

        ## Convert missing
        character <- lapply(character, convert.missing, all_states)

        ## Convert into numeric
        return(lapply(character, function(X) sort(as.numeric(X))))
    }
}

# ' @title Descendants and ancestors
# '
# ' @description Returns the right and left descendants and the ancestor of one node
# '
# ' @param node \code{numeric}, the number of a node
# ' @param tree \code{phylo}, a tree
# '
# ' @author Thomas Guillerme

desc.anc <- function(node, tree) {
    descendants <- tree$edge[which(tree$edge[,1] == node),2]
    ancestor <- tree$edge[which(tree$edge[,2] == node),1]
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

## Set up the right and left actives (special condition if tips)
get.side.applicable <- function(states_matrix, tree, node, side, pass) {

    side <- ifelse(side == "right", 1, 2)

    desc_anc <- desc.anc(node, tree)

    curr_node <- states_matrix[[pass]][[node]]

    if(desc_anc[side] < ape::Ntip(tree)+1) {
        ## Get the tip value
        tip <- states_matrix[[pass+1]][desc_anc[side]][[1]]
        if(length(tip) == 1) {
            ## If the tip has only one state
            side_applicable <- ifelse(any(tip == -1), FALSE, TRUE)
        } else {
            ## If the tip is ambiguous (question mark), solve using the current node
            if(any(tip == -1)) {
                side_applicable <- ifelse(any(curr_node == -1), FALSE, TRUE)  
            } else {
                side_applicable <- TRUE
            }
        }
    } else {
        ## Get the applicability from the node (saved in the tracker)
        side_applicable <- states_matrix$tracker[[pass]][desc_anc[side]][[1]]
    }

    return(side_applicable)
}     
