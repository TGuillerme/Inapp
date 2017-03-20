# library(testthat)
# test_dir("test")

#' @title Convert character
#'
#' @description Convert a character if it is not numeric (transforming - into -1 and ? into all characters (but - ))
#'
#' @param character any character vector
#'
#' @author Thomas Guillerme

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

#' @title Sates matrix
#'
#' @description Creates a states matrix
#'
#' @param tree \code{phylo}, a tree
#' @param character Either vector of character states (\code{"numeric"} or \code{"character"}) or a list of the same length of than the tips in the tree (see details)
#'
#' @details
#' If \code{character} argument is a list, each element of the list must be a \code{"numeric"} vector with \code{"?"} being all states and \code{"-"} being \code{-1}.
#' @author Thomas Guillerme

make.states.matrix <- function(tree, character, inapplicable = NULL) {

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

    ## Add the character into the list
    states_matrix$Char[1:ape::Ntip(tree)] <- character

    ## Set up the active states tracker
    states_matrix$tracker <- list("Dp1" = filling, "Up1" = filling, "Dp2" = filling, "Up2" = filling)

    ## Set a length buffer
    states_matrix$length <- 0

    return(states_matrix)
}

#' @title Descendants and ancestors
#'
#' @description Returns the right and left descendants and the ancestor of one node
#'
#' @param node \code{numeric}, the number of a node
#' @param tree \code{phylo}, a tree
#'
#' @author Thomas Guillerme

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



#' @title Fitch downpass
#'
#' @description Applies a Fitch down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme

fitch.downpass <- function(states_matrix, tree) {

    ## Transferring the characters in the right matrix column
    states_matrix$Dp1 <- states_matrix$Char

    ## Loop through the nodes
    for(node in rev(ape::Ntip(tree)+1:ape::Nnode(tree))) {
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant

        ## Get the states in common between the descendants
        common_desc <- get.common(left, right)

        if(!is.null(common_desc)) {
            ## If there is any states in common, set the node to be that one
            states_matrix$Dp1[[node]] <- common_desc
        } else {
            ## Else set it to be the union of the descendants
            states_matrix$Dp1[[node]] <- get.union.incl(left, right)
           
            ## Increment the tree length
            states_matrix$length <- states_matrix$length + 1
            states_matrix$tracker$Dp1[[node]] <- c(0, states_matrix$tracker$Dp1[[node]])

        }
    }

    return(states_matrix) 
}

#' @title Fitch uppass
#'
#' @description Applies a Fitch up pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme

fitch.uppass <- function(states_matrix, tree) {

    ## Transferring the characters in the right matrix column
    states_matrix$Up1 <- states_matrix$Char

    ## Set up the root
    states_matrix$Up1[[ape::Ntip(tree)+1]] <- states_matrix$Dp1[[ape::Ntip(tree)+1]]

    ## For each node from the root
    for(node in (ape::Ntip(tree)+2:ape::Nnode(tree))) { ## Start past the root (+2)

        curr_node <- states_matrix$Dp1[[node]] # The current node (i.e. prelim)
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant
        ancestor <- states_matrix$Up1[desc_anc[3]][[1]] # The node's ancestor

        ## Get the states in common between the downpass and the ancestor
        common_anc <- get.common(ancestor, curr_node)

        ## If the state in common is the sate of the ancestor
        if(!is.null(common_anc) && length(common_anc) == length(ancestor) && all(common_anc == ancestor)) {
            ## Set final to be common
            states_matrix$Up1[[node]] <- common_anc
        } else {
            ## Get the states in common between the descendants
            common_desc <- get.common(left, right)
            if(!is.null(common_desc)) {
                ## If there is a state in common, set the final to be the union of the prelim and the common state between the ancestor and the union of the descendants
                states_matrix$Up1[[node]] <- get.union.incl(curr_node, get.common(ancestor, get.union.incl(left, right)))
            } else {
                ## Else set the state to be the union between the prelim and the ancestor
                states_matrix$Up1[[node]] <- get.union.incl(curr_node, ancestor)
            }
        }
    }

    return(states_matrix)    
}

#' @title First downpass
#'
#' @description Applies a first down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme

first.downpass <- function(states_matrix, tree) {

    ## Transferring the characters in the right matrix column
    states_matrix$Dp1 <- states_matrix$Char

    ## Set up the root state
    # states_matrix$Up1[[ape::Ntip(tree)+1]] <- states_matrix$Dp1[[ape::Ntip(tree)+1]]

    ## Loop through the nodes
    for(node in rev(ape::Ntip(tree)+1:ape::Nnode(tree))) {
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant

        ## Get the states in common between the descendants
        common_desc <- get.common(left, right)

        if(!is.null(common_desc)) {
            ## If there is any states in common, set the node to be that one
            states_matrix$Dp1[[node]] <- common_desc

            ## If state in common is actually the inapplicable token, but that both descendants have applicables, set it to be the union between the descendants
            if(all(common_desc == -1) && any(left != -1) && any(right != -1)) {
                states_matrix$Dp1[[node]] <- get.union.incl(left, right)
            }
        } else {
            ## Else set it to be the union of the descendants
            states_matrix$Dp1[[node]] <- get.union.incl(left, right)

            ## If both descendants have applicable states, remove the inapplicable state from the node
            if(any(left != -1) && any(right != -1)) {
                states_matrix$Dp1[[node]] <- states_matrix$Dp1[[node]][which(states_matrix$Dp1[[node]] != -1)]
            }
        }
    }

    return(states_matrix)
}

#' @title First uppass
#'
#' @description Applies a first uppass pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme

first.uppass <- function(states_matrix, tree) {

    ## Transferring the characters in the right matrix column
    states_matrix$Up1 <- states_matrix$Char
        
    ## Pre-condition: if the root is inapplicable AND applicable, remove inapplicable (if there's more than 2 states and one -1)
    if(length(states_matrix$Dp1[[ape::Ntip(tree)+1]]) > 1 && any(states_matrix$Dp1[[ape::Ntip(tree)+1]] == -1)) {
        states_matrix$Up1[[ape::Ntip(tree)+1]] <- states_matrix$Dp1[[ape::Ntip(tree)+1]][which(states_matrix$Dp1[[ape::Ntip(tree)+1]] != -1)]
    } else {
        states_matrix$Up1[[ape::Ntip(tree)+1]] <- states_matrix$Dp1[[ape::Ntip(tree)+1]]
    }

    ## For each node from the root
    for(node in (ape::Ntip(tree)+2:ape::Nnode(tree))) { ## Start past the root (+2)

        curr_node <- states_matrix$Dp1[[node]] # The current node
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant
        ancestor <- states_matrix$Up1[desc_anc[3]][[1]] # The node's ancestor

        if(any(curr_node == -1)) {
            ## If any of the states is inapplicable...
            if(any(curr_node != -1)) {
                ## If any of the states IS applicable
                if(any(ancestor == -1)) {
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else {
                    ## Else remove the inapplicable
                    states_matrix$Up1[[node]] <- curr_node[which(curr_node != -1)]
                }
            } else {
                ## No state IS applicable
                if(any(ancestor == -1)) {
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else {
                    ## If the union of left and right has an applicable
                    union_desc <- get.union.incl(left, right)
                    if(any(union_desc != -1)) {
                        ## Set to the union of applicable states
                        states_matrix$Up1[[node]] <- union_desc[which(union_desc != -1)]
                    } else {
                        ## Set to inapplicable
                        states_matrix$Up1[[node]] <- -1
                    }
                }
            }
        } else {
            ## No inapplicable states so don't change any
            states_matrix$Up1[[node]] <- curr_node
        }
    }

    return(states_matrix)
}

#' @title Second downpass
#'
#' @description Applies a second down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme


# # DEBUG
# tree <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
# character <- "23--1??--032"
# # character <- "1---1111---1" # Not activating anything on the uppass? Missing 1 count
# # character <- "1100----0011"
# # character <- "23--1----032"
# # character <- "01---1010101"
# # character <- "210--100--21"

# states_matrix <- make.states.matrix(tree, character, inapplicable = NULL)
# n_passes <- list(first.downpass, first.uppass, second.downpass, second.uppass)
# for (pass in 1:4) {
#         states_matrix <- n_passes[[pass]](states_matrix, tree)
#     }
# get.length(states_matrix)

second.downpass <- function(states_matrix, tree) {
    
    ## Active states
    actives <- NULL

    ## Transferring the characters in the right matrix column
    states_matrix$Dp2 <- states_matrix$Char

    ## Loop through the nodes
    for(node in rev(ape::Ntip(tree)+1:ape::Nnode(tree))) { # flip_clade <- c(18,17,16,15,14,23,22,21,20,19,13)

        curr_node <- states_matrix$Up1[[node]]
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp2[desc_anc[1]][[1]]
        left <- states_matrix$Dp2[desc_anc[2]][[1]]

        if(any(curr_node != -1)) {
            ## Get the states in common between the descendants
            common_desc <- get.common(left, right)

            if(!is.null(common_desc)) {
                ## If there is any applicable state in this common, set the node to be that state
                if(any(common_desc != -1)) {
                    states_matrix$Dp2[[node]] <- common_desc[which(common_desc != -1)]
                } else {
                ## @@@ Changed: Else set the state to the inapplicable token
                    states_matrix$Dp2[[node]] <- -1
                }   
            } else {
                ## Else set the node state to be the union of the descendants without the inapplicable tokens
                union_desc <- get.union.incl(left, right)
                states_matrix$Dp2[[node]] <- union_desc[which(union_desc != -1)]

                ## Counting
                if(any(left != -1) && any(right != -1)) {
                    
                    if(!is.null(actives) && all(states_matrix$Dp2[[node]] %in% get.common(states_matrix$Dp2[[node]], actives))) { #TG: Counting problem here?
                        ## Increment the count
                        states_matrix$length <- states_matrix$length+1
                        # cat(paste("pass 3 - node ", node, ": added length +1 (is now ", states_matrix$length, ")\n", sep = ""))
                        states_matrix$tracker$Dp2[[node]] <- c(1, states_matrix$tracker$Dp2[[node]])
                    } else {
                        ## Activate the states
                        actives <- unique(c(states_matrix$Dp2[[node]], actives))
                        ## Remove inapplicables
                        actives <- sort(actives[which(actives != -1)])
                        # cat(paste("pass 3 - node ", node, ": activated states c1 (actives are now: ", paste(actives, collapse = ", "), ")\n", sep = ""))
                        states_matrix$tracker$Dp2[[node]] <- c(0, states_matrix$tracker$Dp2[[node]])
                    }
                }
            }
        } else {
            ## Else, leave the state as it was after the first uppass
            states_matrix$Dp2[[node]] <- curr_node

            if(is.null(get.common(left, right))) {
                ## Adding activation
                union_desc <- get.union.incl(left, right)
                ## Activate the states
                actives <- unique(c(union_desc, actives))
                ## Remove inapplicables
                actives <- sort(actives[which(actives != -1)])
                # cat(paste("pass 3 - node ", node, ": activated states c2 (actives are now: ", paste(actives, collapse = ", "), ")\n", sep = ""))
                states_matrix$tracker$Dp2[[node]] <- c(0, states_matrix$tracker$Dp2[[node]])
            }
        }
    }

    return(states_matrix)
}

#' @title Second uppass
#'
#' @description Applies a second up pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' @param tree A \code{phylo} tree
#'
#' @author Thomas Guillerme

second.uppass <- function(states_matrix, tree) {

    ## Active states
    actives <- NULL

    ## Transferring the characters in the right matrix column
    states_matrix$Up2 <- states_matrix$Char

    ## Root state is inherited from the second downpass
    # states_matrix$Up2[[ape::Ntip(tree)+1]] <- states_matrix$Dp2[[ape::Ntip(tree)+1]]

    ## For each node from the root
    for(node in (ape::Ntip(tree)+1:ape::Nnode(tree))) {

        curr_node <- states_matrix$Dp2[[node]]
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp2[desc_anc[1]][[1]]
        left <- states_matrix$Dp2[desc_anc[2]][[1]]
        ancestor <- states_matrix$Up2[desc_anc[3]][[1]]

        if(any(curr_node != -1)) {
            if(any(ancestor != -1)) {

                common_anc_node <- get.common(ancestor, curr_node)
                
                if(!is.null(common_anc_node) && length(common_anc_node) == length(ancestor) && all(common_anc_node == ancestor) ){

                    states_matrix$Up2[[node]] <- common_anc_node
                } else {
                    ## If the common state between the ancestor and the final is not the ancestor
                    common_left_right <- get.common(left, right)
                    if(!is.null(common_left_right)) {
                        ## If there is a state in common between left and right
                        long_union <- get.union.incl(common_anc_node, get.common(ancestor, get.union.incl(left, right)))
                        
                        if(!is.null(long_union)) {
                            states_matrix$Up2[[node]] <- long_union
                        } else {
                            states_matrix$Up2[[node]] <- common_left_right
                        }

                    } else {
                        ## If there is no state in common between left and right
                        union_desc <- get.union.incl(left, right)
                        
                        if(any(union_desc == -1)) {

                            if(!is.null(get.common(union_desc, ancestor))) {
                                states_matrix$Up2[[node]] <- get.union.incl(get.common(ancestor, union_desc), ancestor)
                            } else { 
                                ## If the union of left and right has no state in common with the ancestor
                                union_all <- get.union.incl(union_desc, ancestor)
                                states_matrix$Up2[[node]] <- union_all[which(union_all != -1)]
                            }
                        } else {
                            ## If the union of left and right has no inapplicable character
                            union_node_anc <- get.union.incl(curr_node, ancestor)
                            states_matrix$Up2[[node]] <- union_node_anc
                            
                            options(warn = -1)
                            if(all(union_node_anc == ancestor)) {
                                ## If the state in common between the node and the ancestor is the ancestor
                                states_matrix$Up2[[node]] <- get.common(ancestor, states_matrix$Up2[[node]])
                            }
                            options(warn = 0)
                        }
                    }
                }
            } else {
                ## If the ancestor has no applicable state
                common_desc <- get.common(left, right)
                
                if(!is.null(common_desc)) {
                    states_matrix$Up2[[node]] <- common_desc
                } else {
                    states_matrix$Up2[[node]] <- curr_node
                }
            }
        } else {
            ## If there is no applicable state in the previous pass
            states_matrix$Up2[[node]] <- curr_node

            if(is.null(get.common(left, right))) {
                if(!is.null(get.common(get.union.incl(left, right), actives))) {
                    ## Increment the counting
                    states_matrix$length <- states_matrix$length+1
                    # cat(paste("pass 4 - node ", node, ": added length +1 (is now ", states_matrix$length, ")\n", sep = ""))
                    states_matrix$tracker$Up2[[node]] <- c(1, states_matrix$tracker$Up2[[node]])
                }
            }
        }

        if(is.null(get.common(left, right))) {
            ## Adding activation
            union_desc <- get.union.incl(left, right)
            ## Activation
            actives <- unique(c(union_desc, actives))
            ## Remove inapplicables
            actives <- sort(actives[which(actives != -1)])
            # cat(paste("pass 4 - node ", node, ": activated states (actives are now: ", paste(actives, collapse = ", "), ")\n", sep = ""))
            states_matrix$tracker$Up2[[node]] <- c(0, states_matrix$tracker$Up2[[node]])
        }
    }
    return(states_matrix)
}


#' @title Inapplicable algorithm
#'
#' @description Runs a full inapplicable algorithm
#'
#' @param tree \code{phylo}, a tree
#' @param character \code{character}, a vector of character states
#' @param passes \code{numeric}, the number of passes in the tree; from \code{1} to \code{4} (default)
#' @param inapplicable \code{NULL}, \code{1}, \code{2} for respectively treat inapplicable as -, ? or n
#' 
#' @author Thomas Guillerme

inapplicable.algorithm <- function(tree, character, passes = 4, method, inapplicable) {

    ## Setting up the output state matrix
    states_matrix <- make.states.matrix(tree, character, inapplicable)

    ## Setting the list of passes
    if(method == "Inapplicable") {
        n_passes <- list(first.downpass, first.uppass, second.downpass, second.uppass)
    } else {
        n_passes <- list(fitch.downpass, fitch.uppass)
    }

    ## Applying the passes for each node
    for (pass in 1:passes) {
        states_matrix <- n_passes[[pass]](states_matrix, tree)
    }

    return(states_matrix)
}

## Internal plot utility: converts characters (-1,0,n,c(-1,0,n)) into character ("-0n?")
plot.convert.state <- function(character, missing = FALSE) {

    plot.convert.inappli <- function(X) {
        return(ifelse(X == -1, "-", X))
    }

    plot.convert.missing <- function(X, all_states) {
        if(length(X) == length(all_states) && all(X == all_states)) {
            return("?")
        } else {
            return(X)
        }
    }

    if(missing) {
        ## Getting all states
        all_states <- unique(unlist(character))
        ## Convert the missing states
        character <- lapply(character, plot.convert.missing, all_states)
    }

    ## Convert the inapplicables
    character <- lapply(character, plot.convert.inappli)

    ## Convert into character
    return(unlist(lapply(character, function(X) paste(as.character(X), collapse = ""))))
}


#' @title Plot inapplicable algorithm
#'
#' @description Plots the results of the inapplicable algorithm
#'
#' @param tree \code{phylo}, a tree
#' @param character \code{character}, a vector of character states
#' @param passes \code{numeric}, the number of passes to plot (default = \code{c(1,2,3,4)})
#' @param show.tip.label \code{logical}, whether to display tip labels (default = \code{FALSE}).
#' @param col.tips.nodes \code{character}, a vector of one or two colors to be used for displaying respectively the tips and the nodes.
#' @param method \code{"Inapplicable"} for the new algorithm or \code{"Fitch"} for classic Fitch
#' @param inapplicable optional, when \code{method = "Fitch"}, how to treat the inapplicable data (\code{1} = as missing data, \code{2} = as an extra state).
##' @param col.character \code{logical} or \code{character}, whether to display the characters states as colors.
#' @param ... any optional arguments to be passed to \code{\link[ape]{plot.phylo}}
#' 
#' @author Thomas Guillerme

# ## DEBUG
# warning("DEBUG")
# tree <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
# character <- "01---1010101"
# tree <- read.tree(text = "(((1,2),(3,4)),5);")
# character <- "0-2-0"


# plot.inapplicable.algorithm(tree, character)

plot.inapplicable.algorithm <- function(tree, character, passes = c(1,2,3,4), show.tip.label = FALSE, col.tips.nodes = c("orange", "lightblue"), method = "Inapplicable", inapplicable = NULL, ...) {

    ## SANITIZING
    ## tree character done in make.states.matrix
    
    ## Passes
    if(class(passes) != "numeric" | any(is.na(match(passes, c(1,2,3,4))))) {
        stop("passes argument must be any integer(s) between 1 and 4.")
    }
    ## show.tip.label
    if(class(show.tip.label) != "logical") {
        stop("show.tip.label argument must be logical.")
    }
    ## col.tips.nodes
    if(length(col.tips.nodes) == 1) {
        col.tips.nodes <- rep(col.tips.nodes, 2)
    } else {
        if(length(col.tips.nodes) > 2) {
            col.tips.nodes <- col.tips.nodes[1:2]
            warning("Only the two first colors from col.tips.nodes are used.")
        }
    }
    ## method
    if(method != "Inapplicable" && method != "Fitch") {
        stop("method should be 'Fitch' or 'Inapplicable'")
    }
    ## If Fitch, only use two passes
    if(method == "Fitch" && !all(is.na(match(passes, c(3,4)))) ) {
        passes <- c(1,2)
    }
    if(method == "Fitch" && is.null(inapplicable)) {
        inapplicable <- 1
    }

    ## RUN THE STATE RECONSTRUCTION (4 passes)
    if(method == "Inapplicable") {
        states_matrix <- inapplicable.algorithm(tree, character, passes = 4, method = method, inapplicable = inapplicable)
    } else {
        states_matrix <- inapplicable.algorithm(tree, character, passes = 2, method = method, inapplicable = inapplicable)
    }

    ## Get the text plotting size
    # cex <- 1 - (ape::Ntip(tree)/100)
    cex <- 1
    ## Correct if ape::Ntip < 10 or ape::Ntip > 100
    # cex <- ifelse(ape::Ntip(tree) < 10, 1, cex)
    # cex <- ifelse(ape::Ntip(tree) > 100, 0.1, cex)

    ## Plotting the tree
    plot(ape::ladderize(tree, right = FALSE), show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5, ...)
    # plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5) ; warning("DEBUG plot")
    legend("topleft", paste("Tree length is", states_matrix$length),  bty = "n", cex = 1.2)

    ## Add the tip states
    if(class(character) == "character" && length(character) == 1) {
        ape::tiplabels(as.character(strsplit(as.character(character), "")[[1]]), cex = cex, bg = col.tips.nodes[1], adj = 1)
    } else {
        tips_labels <- plot.convert.state(states_matrix[[1]][1:ape::Ntip(tree)], missing = TRUE)
        ape::tiplabels(tips_labels, cex = 1, bg = col.tips.nodes[1], adj = 1)
    }

    ## ADD THE NODE LABELS

    if(length(passes) > 0) {
        node_labels <- plot.convert.state(states_matrix[[passes[1]+1]][-c(1:ape::Ntip(tree))])
        node_labels <- paste(paste(passes[1], ":", sep = ""), node_labels)
        for(pass in passes[-1]) {
            node_labels <- paste(node_labels, paste(pass, ": ", plot.convert.state(states_matrix[[pass + 1]][-c(1:ape::Ntip(tree))]), sep = ""), sep = "\n")
        }
        ape::nodelabels(node_labels, cex = cex-(1-cex)*(length(passes)*0.85), bg = col.tips.nodes[2])
    }


    return(invisible())
}
