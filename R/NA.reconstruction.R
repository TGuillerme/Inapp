#' @title First downpass
#'
#' @description Applies a first down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' 
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' NA_matrix <- make.states.matrix(tree, "01?-")
#' 
#' ## Apply the first downpass
#' (NA_matrix <- first.downpass(NA_matrix))
#' 
#' ## Access the states
#' NA_matrix$Dp1
#'
#' @seealso \code{\link{apply.reconstruction}}
#' 
#' @author Thomas Guillerme
#' @export

first.downpass <- function(states_matrix) {

    tree <- states_matrix$tree

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
                states_matrix$Dp1[[node]] <- states_matrix$Dp1[[node]][states_matrix$Dp1[[node]] != -1]
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
#' 
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' NA_matrix <- make.states.matrix(tree, "01?-")
#' 
#' ## Apply the first uppass
#' (NA_matrix <- first.uppass(first.downpass(NA_matrix)))
#' 
#' ## Access the states
#' NA_matrix$Up1
#' 
#' @seealso \code{\link{apply.reconstruction}}
#' 
#' @author Thomas Guillerme
#' @export

first.uppass <- function(states_matrix) {

    tree <- states_matrix$tree

    ## Transferring the characters in the right matrix column
    states_matrix$Up1 <- states_matrix$Char
        
    ## Pre-condition: if the root is inapplicable AND applicable, remove inapplicable (if there's more than 2 states and one -1)
    if(length(states_matrix$Dp1[[ape::Ntip(tree)+1]]) > 1 && any(states_matrix$Dp1[[ape::Ntip(tree)+1]] == -1)) {
        states_matrix$Up1[[ape::Ntip(tree)+1]] <- states_matrix$Dp1[[ape::Ntip(tree)+1]][states_matrix$Dp1[[ape::Ntip(tree)+1]] != -1]
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
                if(any(ancestor == -1)) { #TG: change to to (ancestor == -1) (not any()) At this stage the ancestor is either - or A (not -A) #TG: fix, checked with Martin.
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else {
                    ## Else remove the inapplicable
                    states_matrix$Up1[[node]] <- curr_node[curr_node != -1]
                }
            } else {
                ## No state IS applicable
                if(any(ancestor == -1)) { #TG: change to to (ancestor == -1) (not any()) At this stage the ancestor is either - or A (not -A) #TG: fix, checked with Martin.
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else {
                    ## If the union of left and right has an applicable
                    union_desc <- get.union.incl(left, right)
                    if(any(union_desc != -1)) {
                        ## Set to the union of applicable states
                        states_matrix$Up1[[node]] <- union_desc[union_desc != -1]
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
#' 
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' NA_matrix <- make.states.matrix(tree, "01?-")
#' 
#' ## Apply the second downpass
#' (NA_matrix <- second.downpass(first.uppass(first.downpass(NA_matrix))))
#' 
#' ## Access the states
#' NA_matrix$Dp2
#' 
#' @seealso \code{\link{apply.reconstruction}}
#' 
#' @author Thomas Guillerme
#' @export

second.downpass <- function(states_matrix) {

    tree <- states_matrix$tree
    
    ## Transferring the characters in the right matrix column
    states_matrix$Dp2 <- states_matrix$Char

    ## Loop through the nodes
    for(node in rev(ape::Ntip(tree)+1:ape::Nnode(tree))) {

        curr_node <- states_matrix$Up1[[node]]
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp2[desc_anc[1]][[1]]
        left <- states_matrix$Dp2[desc_anc[2]][[1]]

        ## Get the actives
        right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
        left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)

        ## Record the region tracker for displaying later
        states_matrix$tracker$Dp2[desc_anc[1]][[1]] <- right_applicable
        states_matrix$tracker$Dp2[desc_anc[2]][[1]] <- left_applicable
        states_matrix$tracker$Dp2[node][[1]] <- left_applicable | right_applicable

        if(any(curr_node != -1)) {
            ## Get the states in common between the descendants
            common_desc <- get.common(left, right)

            if(!is.null(common_desc)) {
                ## If there is any applicable state in this common, set the node to be that state
                if(any(common_desc != -1)) {
                    states_matrix$Dp2[[node]] <- common_desc[common_desc != -1]
                } else {
                    states_matrix$Dp2[[node]] <- -1
                }   
            } else {
                ## Else set the node state to be the union of the descendants without the inapplicable tokens
                union_desc <- get.union.incl(left, right)
                states_matrix$Dp2[[node]] <- union_desc[union_desc != -1]

                ## Counting
                if(any(left != -1) && any(right != -1)) {
                    ## Store the node
                    states_matrix$changes <- c(states_matrix$changes, node)
                } else {
                    if(right_applicable && left_applicable) {
                        states_matrix$regions <- states_matrix$regions + 1
                    }
                }

            }
        } else {
            ## Else, leave the state as it was after the first uppass
            states_matrix$Dp2[[node]] <- curr_node
        }

    }

    return(states_matrix)
}



#' @title Second uppass
#'
#' @description Applies a second up pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#' 
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' NA_matrix <- make.states.matrix(tree, "01?-")
#' 
#' ## Apply the second uppass
#' (NA_matrix <- second.uppass(second.downpass(first.uppass(first.downpass(NA_matrix)))))
#' 
#' ## Access the states
#' NA_matrix$Up2
#' 
#' @seealso \code{\link{apply.reconstruction}}
#' 
#' @author Thomas Guillerme
#' @export

second.uppass <- function(states_matrix) {

    tree <- states_matrix$tree

    ## Transferring the characters in the right matrix column
    states_matrix$Up2 <- states_matrix$Char

    ## For each node from the root
    for(node in (ape::Ntip(tree)+1:ape::Nnode(tree))) {

        curr_node <- states_matrix$Dp2[[node]]
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp2[desc_anc[1]][[1]]
        left <- states_matrix$Dp2[desc_anc[2]][[1]]
        ancestor <- states_matrix$Up2[desc_anc[3]][[1]]

        ## Get the actives
        right_applicable <- states_matrix$tracker$Dp2[desc_anc[1]][[1]]
        left_applicable <- states_matrix$tracker$Dp2[desc_anc[2]][[1]]

        ## Record the region tracker for displaying later
        states_matrix$tracker$Up2[desc_anc[1]][[1]] <- right_applicable
        states_matrix$tracker$Up2[desc_anc[2]][[1]] <- left_applicable
        states_matrix$tracker$Up2[node][[1]] <- left_applicable | right_applicable

        if(any(curr_node != -1)) {
            if(any(ancestor != -1)) {

                common_anc_node <- get.common(ancestor, curr_node)
                
                if(!is.null(common_anc_node) && length(common_anc_node) == length(ancestor) && all(common_anc_node == ancestor) ){

                    states_matrix$Up2[[node]] <- common_anc_node #TG: simplify in english version
                } else {
                    ## If the common state between the ancestor and the final is not the ancestor
                    common_left_right <- get.common(left, right)
                    if(!is.null(common_left_right)) {
                        ## If there is a state in common between left and right
                        states_matrix$Up2[[node]] <- get.union.incl(curr_node, get.common(ancestor, get.union.incl(left, right)))
                        #TG: in english: add to current node what's common between ancestor and descendants

                    } else {
                        ## If there is no state in common between left and right
                        union_desc <- get.union.incl(left, right)
                        
                        if(any(union_desc == -1)) {

                            if(!is.null(get.common(union_desc, ancestor))) {
                                states_matrix$Up2[[node]] <- ancestor
                            } else { 
                                ## If the union of left and right has no state in common with the ancestor
                                union_all <- get.union.incl(union_desc, ancestor)
                                states_matrix$Up2[[node]] <- union_all[union_all != -1]
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
                states_matrix$Up2[[node]] <- curr_node
            }
        } else {
            ## If there is no applicable state in the previous pass
            states_matrix$Up2[[node]] <- curr_node

            ## Counting
            if(right_applicable && left_applicable) {
                states_matrix$regions <- states_matrix$regions + 1
            }
        }
    }
    return(states_matrix)
}