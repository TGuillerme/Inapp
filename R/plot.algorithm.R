#' @title Plot tree and ancestral states
#'
#' @description Plots an ancestral states reconstruction and tree score
#'
#' @param states_matrix A \code{states.matrix} list from \code{\link{apply.reconstruction}}
#' @param passes \code{numeric}, the number of passes to plot (default = \code{c(1,2,3,4)})
#' @param show.labels \code{numeric}, either \code{1} for showing the tip labels, \code{2} for the node labels or \code{c(1,2)} for both (default = \code{NULL}).
#' @param col.tips.nodes \code{character}, a vector of up to three colors to be used for displaying respectively the tips, the nodes and the activated/counted nodes (if \code{counts != 0}).
#' @param counts \code{numeric}, whether to display the activations (\code{1}) or/and the homoplasies (\code{2}) or nothing (\code{0}; default).
#' @param use.edge.length \code{logical} indicating whether to use the edge lengths of the phylogeny to draw the branches or not (default).
#' @param ... any optional arguments to be passed to \code{\link[ape]{plot.phylo}}
#' 
#' @examples
#' ## A balanced 12 taxa tree
#' tree <- ape::read.tree(
#'                  text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
#' ## A character with inapplicable data
#' character <- "23--1??--032"
#' 
#' ## NA algorithm
#' NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
#' 
#' ## Plotting the tree and the states
#' plot(NA_matrix)
#' 
#' ## Plotting the tree and the states with the state changes and regions
#' plot(NA_matrix, counts = c(1,2))
#' 
#' ## Plot the tree with tip/node labels, and only the 1st and 2nd downpass
#' plot(NA_matrix, passes = c(1,3), show.labels = c(1,2))
#' 
#' ## Plot the tree only the 2nd uppass with the state changes in green
#' plot(NA_matrix, show.labels = 2, col.tips.nodes = c("red", "pink", "green"),
#'      counts = c(1,2), passes = c(3,4))
#' 
#' @seealso \code{\link{apply.reconstruction}}, \code{\link{runInapp}}
#' 
#' @author Thomas Guillerme
#' @export

plot.states.matrix <- function(states_matrix, passes = c(1,2,3,4), show.labels = 0, col.tips.nodes = c("orange", "bisque2", "lightblue"), counts = 0, use.edge.length = FALSE, ...) {

    tree <- states_matrix$tree

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
    if(!(class(passes) %in% c("numeric", "integer")) || any(is.na(match(passes, c(1,2,3,4))))) {
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
    if(any(counts == 1) && !is.null(unlist(states_matrix$Up2))) {
        ## Change the colors of the edges' if activations exist (and if the algorithm is NA)
        edge_col <- ifelse(get.NA.edges(states_matrix, tree, pass = 4) == 1, "black", "grey")
    }

    ## Plotting the tree
    graphics::plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = use.edge.length, cex = cex, adj = 0.5, edge.color = edge_col, edge.width = 2, ...)
    # plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5, edge.color = edge_col,  edge.width = 2) ; warning("DEBUG plot")


    ## Setting up the legend parameters
    length_text <-  paste("Tree score is", score.from(states_matrix$regions) + score.from(states_matrix$changes))
    if(all(counts == 0)) {
        legend_text <- length_text
        par_cex = 0
        par_pch = 0
        par_lty = 0
        par_lwd = 0
        par_col = "white"
    } else {
        if(all(counts == 1)) {
            legend_text <- c(length_text, paste("applicable region (1 + ", score.from(states_matrix$regions), ")", sep = ""))
            par_cex = c(0, 0)
            par_pch = c(0, 0)
            par_lty = c(0, 1)
            par_lwd = c(0, 2)
            par_col = "black"
        } else {
            if(all(counts == 2)) {
                legend_text <- c(length_text, paste("state changes (", score.from(states_matrix$changes), ")", sep = ""))
                par_cex = c(0, 2)
                par_pch = c(0, 15)
                par_lty = c(0, 0)
                par_lwd = c(0, 0)
                par_col = col.tips.nodes[3]
            } else {
                if(all(counts %in% c(1,2))) {
                    legend_text <- c(length_text, paste("applicable region (1 + ", score.from(states_matrix$regions), ")", sep = ""), paste("state changes (", length(states_matrix$changes), ")", sep = ""))
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
    graphics::legend("topleft", legend = legend_text, cex = 1.2, pch = par_pch, lty = par_lty,
                     lwd = par_lwd, col = par_col, pt.cex = par_cex, x.intersp = 0.5, 
                     bty='n', bg = NULL)

    ## Add the tip states
    tips_labels <- plot.convert.state(states_matrix[[1]][1:states_matrix$n_tip], missing = TRUE)
    ape::tiplabels(tips_labels, cex = 1, bg = col.tips.nodes[1], adj = 1)


    ## ADD THE NODE LABELS

    if(length(passes) > 0) {

        ## Get the first set of node labels
        node_labels <- plot.convert.state(states_matrix[[passes[1]+1]][-c(1:states_matrix$n_tip)])
        node_labels <- paste(paste(passes[1], ":", sep = ""), node_labels)

        ## Adding node numbers (optional)
        if(show.node.label) {
            node_labels <- paste(paste("n",(states_matrix$n_tip+1):(states_matrix$n_tip + states_matrix$n_node), "\n", sep = ""), node_labels, sep = "")
        }
        
        ## Add the extra node labels
        for(pass in passes[-1]) {
            node_labels <- paste(node_labels, paste(pass, ": ", plot.convert.state(states_matrix[[pass + 1]][-c(1:states_matrix$n_tip)]), sep = ""), sep = "\n")
        }

        ## Set the colors for the changes
        if(any(counts == 2)) {
            ## Change the colors of the nodes if activations exist
            if(length(states_matrix$changes) > 0) {
                bg_col <- rep(col.tips.nodes[2], states_matrix$n_node)
                bg_col[states_matrix$changes - states_matrix$n_tip] <- col.tips.nodes[3]
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
