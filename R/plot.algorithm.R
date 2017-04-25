#' @title Inapplicable algorithm
#'
#' @description Runs a full inapplicable algorithm
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

NA.algorithm <- function(tree, character, passes = 4, method, inapplicable, match.tip.char = FALSE) {

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
        states_matrix <- n_passes[[pass]](states_matrix, tree)
    }

    class(states_matrix) <- "NA.matrix"

    return(states_matrix)
}

#' @title Plot inapplicable algorithm
#'
#' @description Plots the results of the inapplicable algorithm
#'
#' @param states_matrix \code{list}, a list of state changes from \code{inapplicable.algorithm}
#' @param tree \code{phylo}, the tree used in the analysis
#' @param passes \code{numeric}, the number of passes to plot (default = \code{c(1,2,3,4)})
#' @param show.labels \code{numeric}, either \code{1} for showing the tip labels, \code{2} for the node labels or \code{c(1,2)} for both (default = \code{NULL}).
#' @param col.tips.nodes \code{character}, a vector of up to three colors to be used for displaying respectively the tips, the nodes and the activated/counted nodes (if \code{counts != 0}).
#' @param counts \code{numeric}, whether to display the activations (\code{1}) or/and the homoplasies (\code{2}) or nothing (\code{0}; default).
#' @param ... any optional arguments to be passed to \code{\link[ape]{plot.phylo}}
#' 
#' @author Thomas Guillerme
#' @export

plot.NA.matrix <- function(states_matrix, tree, passes = c(1,2,3,4), show.labels = 0, col.tips.nodes = c("orange", "bisque2", "lightblue"), counts = 0, ...) {


    ## Internal plot utility: converts characters (-1,0,n,c(-1,0,n)) into character ("-0n?")
    plot.convert.state <- function(character, missing = FALSE) {

        plot.convert.inappli <- function(X) {
            return(ifelse(X == -1, "-", X))
        }

        plot.convert.missing <- function(X, all_states) {
            if(length(X) == length(all_states) && all(sort(X) == sort(all_states))) {
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

    ## Internal plot utility: get the inapplicable regions
    get.NA.edges <- function(states_matrix, tree, pass = 4) {

        ## Checking if an edge is applicable (1) or not (0) - edge are inapplicable if both nodes (or tips) at each side are -1
        check.applicable <- function(nodes, states_matrix, pass) {
            node1 <- states_matrix[[pass+1]][nodes[1]][[1]]
            node2 <- states_matrix[[pass+1]][nodes[2]][[1]]

            ## Solving missing data
            all_char <- sort(unique(unlist(states_matrix$Char)))
            options(warn = -1)
            node2 <- ifelse(all(node2 == all_char), node1, node2)
            options(warn = 0)

            ## Getting the branch applicability
            return(ifelse(all(c(node1, node2) == -1), 0, 1))
        }

        ## Check applicability on all the edges
        return(apply(tree$edge, 1, check.applicable, states_matrix, pass))
    }

    ## SANITIZING
    ## tree character done in make.states.matrix
    
    ## Passes
    if(class(passes) != "numeric" || any(is.na(match(passes, c(1,2,3,4))))) {
        stop("passes argument must be any integer(s) between 1 and 4.")
    }
    ## show.labels
    if(!is.null(show.labels)) {
        if(class(show.labels) != "numeric") {
            stop("show.labels argument must be either 1 for tips, 2 for nodes or c(1,2) for both")
        }
        ## Setting up the labels options
        show.tip.label <- ifelse(any(1 %in% show.labels) ,TRUE, FALSE)
        show.node.label <- ifelse(any(2 %in% show.labels) ,TRUE, FALSE)
    } else {
        show.tip.label <- show.node.label <- FALSE
    }

    ## col.tips.nodes
    if(length(col.tips.nodes) == 1) {
        col.tips.nodes <- rep(col.tips.nodes, 3)
    } else {
        if(length(col.tips.nodes) > 3) {
            col.tips.nodes <- col.tips.nodes[1:3]
            warning("Only the two first colors from col.tips.nodes are used.")
        }
    }

    ## counts
    if(any(counts != 0) && !(counts %in% c(1,2))) {
        stop("counts argument must be either 1 for displaying activations and/or 2 for homoplasies.")
    }


    ## Get the text plotting size
    cex <- 1

    ## Set the edges' colors
    edge_col <- "black"
    if(any(counts == 1)) {
        ## Change the colors of the nodes if activations exist (and if the algorithm is NA)
        edge_col <- ifelse(get.NA.edges(states_matrix, tree, pass = 4) == 1, "black", "grey")
    }

    ## Plotting the tree
    graphics::plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5, edge.color = edge_col, edge.width = 2, ...)
    # plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5, edge.color = edge_col,  edge.width = 2) ; warning("DEBUG plot")


    ## Setting up the legend parameters
    length_text <-  paste("Tree length is", states_matrix$regions + ifelse(length(states_matrix$changes) > 0, length(states_matrix$changes), 0))
    if(all(counts == 0)) {
        legend_text <- length_text
        par_cex = 0
        par_pch = 0
        par_lty = 0
        par_lwd = 0
        par_col = "white"
    } else {
        if(all(counts == 1)) {
            legend_text <- c(length_text, paste("applicable region (1 + ", states_matrix$regions, ")", sep = ""))
            par_cex = c(0, 0)
            par_pch = c(0, 0)
            par_lty = c(0, 1)
            par_lwd = c(0, 2)
            par_col = "black"
        } else {
            if(all(counts == 2)) {
                legend_text <- c(length_text, paste("state changes (", length(states_matrix$changes), ")", sep = ""))
                par_cex = c(0, 2)
                par_pch = c(0, 15)
                par_lty = c(0, 0)
                par_lwd = c(0, 0)
                par_col = col.tips.nodes[3]
            } else {
                if(all(counts %in% c(1,2))) {
                    legend_text <- c(length_text, paste("applicable region (1 + ", states_matrix$regions, ")", sep = ""), paste("state changes (", length(states_matrix$changes), ")", sep = ""))
                    par_cex = c(0, 0, 2)
                    par_pch = c(0, 0, 15)
                    par_lty = c(0, 1, 0)
                    par_lwd = c(0, 2, 0)
                    par_col = c("white", "black", col.tips.nodes[3])
                }
            }
        }
    }

    ## Adding the legend
    graphics::legend("topleft", legend = legend_text, cex = 1.2, pch = par_pch, lty = par_lty, lwd = par_lwd, col = par_col, pt.cex = par_cex, x.intersp = 0.5, box.lwd = 0,box.col = "white", bg = "white")

    ## Add the tip states
    tips_labels <- plot.convert.state(states_matrix[[1]][1:ape::Ntip(tree)], missing = TRUE)
    ape::tiplabels(tips_labels, cex = 1, bg = col.tips.nodes[1], adj = 1)


    ## ADD THE NODE LABELS

    if(length(passes) > 0) {

        ## Get the first set of node labels
        node_labels <- plot.convert.state(states_matrix[[passes[1]+1]][-c(1:ape::Ntip(tree))])
        node_labels <- paste(paste(passes[1], ":", sep = ""), node_labels)

        ## Adding node numbers (optional)
        if(show.node.label) {
            node_labels <- paste(paste("n",(ape::Ntip(tree)+1):(ape::Ntip(tree) + ape::Nnode(tree)), "\n", sep = ""), node_labels, sep = "")
        }
        
        ## Add the extra node labels
        for(pass in passes[-1]) {
            node_labels <- paste(node_labels, paste(pass, ": ", plot.convert.state(states_matrix[[pass + 1]][-c(1:ape::Ntip(tree))]), sep = ""), sep = "\n")
        }

        ## Set the colors for the changes
        if(any(counts == 2)) {
            ## Change the colors of the nodes if activations exist
            if(length(states_matrix$changes) > 0) {
                bg_col <- rep(col.tips.nodes[2], ape::Nnode(tree))
                bg_col[states_matrix$changes - ape::Ntip(tree)] <- col.tips.nodes[3]
            } else {
                bg_col <- col.tips.nodes[2]
            }
        } else {
            bg_col <- col.tips.nodes[2]
        }

        ## Plot the node labels
        ape::nodelabels(node_labels, cex = 0.90, bg = bg_col)
        # ape::nodelabels(node_labels, cex = 0.5, bg = bg_col) ; warning("DEBUG")
    }

    return(invisible())
}
