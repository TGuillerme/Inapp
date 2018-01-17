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
    # states_matrix$Up1[[states_matrix$n_tip+1]] <- states_matrix$Dp1[[states_matrix$n_tip+1]]

    ## Loop through the nodes
    for(node in rev(states_matrix$n_tip+1:states_matrix$n_node)) {
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant

        ## Get the states in common between the descendants
        common_desc <- get.common(left, right)

        if(!is.null(common_desc)) { #TG: enter
            ## If state in common is actually the inapplicable token, but that both descendants have applicables, set it to be the union between the descendants
            if(all(common_desc == -1) && any(left != -1) && any(right != -1)) { #TG: AND
                states_matrix$Dp1[[node]] <- get.union.incl(left, right)
            } else {
                states_matrix$Dp1[[node]] <- common_desc
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
    if(length(states_matrix$Dp1[[states_matrix$n_tip+1]]) > 1 && any(states_matrix$Dp1[[states_matrix$n_tip+1]] == -1)) {
        states_matrix$Up1[[states_matrix$n_tip+1]] <- states_matrix$Dp1[[states_matrix$n_tip+1]][states_matrix$Dp1[[states_matrix$n_tip+1]] != -1]
    } else {
        states_matrix$Up1[[states_matrix$n_tip+1]] <- states_matrix$Dp1[[states_matrix$n_tip+1]]
    }

    ## For each node from the root
    for(node in (states_matrix$n_tip+2:states_matrix$n_node)) { ## Start past the root (+2)

        curr_node <- states_matrix$Dp1[[node]] # The current node
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp1[desc_anc[1]][[1]] # The node's right descendant
        left <- states_matrix$Dp1[desc_anc[2]][[1]] # The node's left descendant
        ancestor <- states_matrix$Up1[desc_anc[3]][[1]] # The node's ancestor

        if(any(curr_node == -1)) { #TG: enter
            ## If any of the states is inapplicable...
            if(any(curr_node != -1)) { #TG: NA1
                ## If any of the states IS applicable
                if(any(ancestor == -1)) { #TG: NAandA
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else {
                    ## Else remove the inapplicable
                    states_matrix$Up1[[node]] <- curr_node[curr_node != -1]
                }
            } else { #TG: NA2
                ## No state IS applicable
                if(any(ancestor == -1)) { 
                    ## If the ancestor state has an inapplicable token
                    states_matrix$Up1[[node]] <- -1
                } else { #TG: NodeNAOR
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
    for(node in states_matrix$n_tip+states_matrix$n_node:1) {


        # if(node == 136) {
        #     return(states_matrix)
        # }

        curr_node <- states_matrix$Up1[[node]]
        ## Select the descendants and ancestors
        desc_anc <- desc.anc(node, tree)
        right <- states_matrix$Dp2[desc_anc[1]][[1]]
        left <- states_matrix$Dp2[desc_anc[2]][[1]]

        # ## Get the actives
        # right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
        # left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)

        # ## Record the region tracker for displaying later
        # states_matrix$tracker$Dp2[desc_anc[1]][[1]] <- right_applicable
        # states_matrix$tracker$Dp2[desc_anc[2]][[1]] <- left_applicable
        # states_matrix$tracker$Dp2[node][[1]] <- left_applicable | right_applicable

        if(any(curr_node != -1)) { #TG: \ref Node apply
            ## Get the states in common between the descendants
            common_desc <- get.common(left, right) 

            if(!is.null(common_desc)) { #TG: \ref enter
                ## If there is any applicable state in this common, set the node to be that state
                if(any(common_desc != -1)) { #TG: \ref AND
                    states_matrix$Dp2[[node]] <- common_desc[common_desc != -1]
                    ## Get the actives
                    right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
                    left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)

                } else {
                    states_matrix$Dp2[[node]] <- -1
                    ## Get the actives
                    right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
                    left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)
                }
            } else { #TG: \ref OR
                ## Else set the node state to be the union of the descendants without the inapplicable tokens
                union_desc <- get.union.incl(left, right)
                states_matrix$Dp2[[node]] <- union_desc[union_desc != -1]

                ## Get the actives
                right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
                left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)

                ## Counting
                if(any(left != -1) && any(right != -1)) { #TG:\ref countcchange
                    ## Store the node
                    states_matrix$changes <- c(states_matrix$changes, node)
                } else { #TG:\ref count regions
                    if(right_applicable && left_applicable) {
                        if(any(desc_anc[1:2] > states_matrix$n_tip)) {
                            ## Increment the counting only if the region is depending on at least one node (i.e. ignore tips)
                            states_matrix$regions <- c(states_matrix$regions, node)
                        }
                    }
                }

            }
        } else {
            ## Else, leave the state as it was after the first uppass
            states_matrix$Dp2[[node]] <- curr_node

            ## Get the actives
            right_applicable <- get.side.applicable(states_matrix, node = node, side = "right", pass = 3)
            left_applicable <- get.side.applicable(states_matrix, node = node, side = "left", pass = 3)

            
            ## #MS: \ref CountRegion
            if (left_applicable && right_applicable && all(curr_node == -1)) {
                states_matrix$regions <- c(states_matrix$regions, node)
                states_matrix$downpassRegions <- c(states_matrix$downpassRegions, node) # MS TESTING LINE - TODO DELETE
            }
        
        }

        ## Record the region tracker for displaying later
        states_matrix$tracker$Dp2[desc_anc[1]][[1]] <- right_applicable
        states_matrix$tracker$Dp2[desc_anc[2]][[1]] <- left_applicable
        states_matrix$tracker$Dp2[node][[1]] <- left_applicable | right_applicable


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
    for(node in (states_matrix$n_tip+1:states_matrix$n_node)) {

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

        if(any(curr_node != -1)) { #TG: \ref enter
            if(any(ancestor != -1)) { #TG: \ref nodeA

                common_anc_node <- get.common(ancestor, curr_node)
                
                if(!is.null(common_anc_node) && length(common_anc_node) == length(ancestor) && all(common_anc_node == ancestor) ){ #TG: \ref ancestorA1

                    states_matrix$Up2[[node]] <- common_anc_node 
                } else { #TG: \ref ancestorA2
                    ## If the common state between the ancestor and the final is not the ancestor
                    common_left_right <- get.common(left, right)
                    if(!is.null(common_left_right)) { #TG: \ref ANDdesc
                        ## If there is a state in common between left and right
                        states_matrix$Up2[[node]] <- get.union.incl(curr_node, get.common(ancestor, get.union.incl(left, right)))
                        #TG: in english: add to current node what's common between ancestor and descendants

                    } else {  #TG: \ref ORdesc
                        ## If there is no state in common between left and right
                        union_desc <- get.union.incl(left, right)
                        
                        if(any(union_desc == -1)) { #TG: \ref ORdescNA

                            if(!is.null(get.common(union_desc, ancestor))) {
                                states_matrix$Up2[[node]] <- ancestor
                            } else { 
                                ## If the union of left and right has no state in common with the ancestor
                                union_all <- get.union.incl(union_desc, ancestor)
                                states_matrix$Up2[[node]] <- union_all[union_all != -1]
                            }
                        } else { #TG: \ref ORdescA
                            ## If the union of left and right has no inapplicable character
                            union_node_anc <- get.union.incl(curr_node, ancestor)
                            states_matrix$Up2[[node]] <- union_node_anc
                            
                        }
                    }
                }
            } else { #TG: \ref ancestorNA
                states_matrix$Up2[[node]] <- curr_node
            }
        } else {  #TG: \ref Count Regions
            ## If there is no applicable state in the previous pass
            states_matrix$Up2[[node]] <- curr_node

            ## Counting
            if(right_applicable && left_applicable) {
                if(any(desc_anc[1:2] > states_matrix$n_tip)) {
                    ## Increment the counting only if the region is depending on at least one node (i.e. ignore tips)
                    states_matrix$uppassRegions <- c(states_matrix$uppassRegions, node)
                }
            }
        }
    }
    return(states_matrix)
}



## Set up the right and left actives (special condition if tips)
get.side.applicable <- function(states_matrix, node, side, pass) {

    ## Select the side
    side <- ifelse(side == "right", 1, 2)

    ## Select the descendants and ancestors
    desc_anc <- desc.anc(node, states_matrix$tree)

    ## Select the current node value
    curr_node <- states_matrix[[pass+1]][[node]] #TG +1 is because the first element of the list is $Char (same below)

    ## Check the side's applicability
    if(desc_anc[side] < ape::Ntip(states_matrix$tree)+1) {
        ## Get the tip value
        tip <- states_matrix[[pass+1]][desc_anc[side]][[1]]
        if(length(tip) == 1) {
            ## If the tip has only one state
            side_applicable <- !any(tip == -1)
        } else {
            ## If the tip is ambiguous (question mark), solve using the current node
            if(any(tip == -1)) {
                side_applicable <- !any(curr_node == -1)
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