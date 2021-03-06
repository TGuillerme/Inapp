#' @title Fitch downpass
#'
#' @description Applies a Fitch down pass to a node
#'
#' @param states_matrix A \code{list} contains all the states and the activations
#'
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' matrix <- make.states.matrix(tree, "01?-", inapplicable = 1)
#'
#' ## Apply the fitch downpass
#' (matrix <- fitch.downpass(matrix))
#'
#' ## Access the states
#' matrix$Dp1
#'
#' @seealso \code{\link{apply.reconstruction}}
#'
#' @author Thomas Guillerme
#' @export

fitch.downpass <- function(states_matrix) {

    tree <- states_matrix$tree

    ## Transferring the characters in the right matrix column
    states_matrix$Dp1 <- states_matrix$Char

    ## Loop through the nodes
    for(node in rev(states_matrix$n_tip + seq_len(states_matrix$n_node))) {
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant

        ## Get the states in common between the descendants
        common_desc <- intersect(left, right)

        if(length(common_desc) > 0) {
            ## If there is any states in common, set the node to be that one
            states_matrix$Dp1[[node]] <- sort(common_desc)
        } else {
            ## Else set it to be the union of the descendants
            states_matrix$Dp1[[node]] <- sort(union(left, right))

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
#' @examples
#' ## Create a states matrix for reconstruction
#' tree <- ape::read.tree(text = "((a,b),(c,d));")
#' matrix <- make.states.matrix(tree, "01?-", inapplicable = 1)
#'
#' ## Apply the fitch downpass
#' (matrix <- fitch.uppass(fitch.downpass(matrix)))
#'
#' ## Access the states
#' matrix$Up1
#'
#' @seealso \code{\link{apply.reconstruction}}
#'
#' @author Thomas Guillerme
#' @export

fitch.uppass <- function(states_matrix) {

    tree <- states_matrix$tree

    ## Transferring the characters in the right matrix column
    states_matrix$Up1 <- states_matrix$Char

    ## Set up the root
    states_matrix$Up1[[states_matrix$n_tip+1]] <- states_matrix$Dp1[[states_matrix$n_tip+1]]

    ## For each node from the root
    for(node in (states_matrix$n_tip+2:states_matrix$n_node)) { ## Start past the root (+2)

        curr_node <- states_matrix$Dp1[[node]] # The current node (i.e. prelim)
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant
        ancestor <- states_matrix$Up1[desc_anc[3]][[1]] # The node's ancestor

        ## Get the states in common between the downpass and the ancestor
        common_anc <- intersect(ancestor, curr_node)

        ## If the state in common is the state of the ancestor
        if(length(common_anc) > 0 &&
           length(common_anc) == length(ancestor) &&
           all(common_anc == ancestor)) {
            ## Set final to be common
            states_matrix$Up1[[node]] <- common_anc
        } else {
            ## Get the states in common between the descendants
            common_desc <- intersect(left, right)
            if(length(common_desc) > 0) {
                ## If there is a state in common, set the final to be the union of the prelim and the common state between the ancestor and the union of the descendants
                states_matrix$Up1[[node]] <-
                    sort(union(curr_node, intersect(ancestor, union(left, right))))
            } else {
                ## Else set the state to be the union between the prelim and the ancestor
                states_matrix$Up1[[node]] <- sort(union(curr_node, ancestor))
            }
        }
    }

    return(states_matrix)
}
