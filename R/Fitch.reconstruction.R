#' @title Fitch downpass
#'
#' @description Applies a Fitch down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#'
#' @author Thomas Guillerme
#' @export

fitch.downpass <- function(states_matrix) {

    tree <- states_matrix$tree

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
           
            ## Store the node
            states_matrix$changes <- c(states_matrix$changes, node)
        }
    }

    return(states_matrix) 
}

#' @title Fitch uppass
#'
#' @description Applies a Fitch up pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#'
#' @author Thomas Guillerme
#' @export

fitch.uppass <- function(states_matrix) {

    tree <- states_matrix$tree

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
