#' @title Plot tree and ancestral states
#'
#' @description Plots an ancestral states reconstruction and tree score
#'
#' @param x A \code{states.matrix} list from \code{\link{apply.reconstruction}}
#' @param passes \code{numeric}, the number of passes to plot (default = \code{c(1,2,3,4)}).
#'               Set to 0 to leave nodes unlabelled.
#' @param show.labels \code{numeric}, either \code{1} for showing the tip labels, \code{2} for the node labels or \code{c(1,2)} for both (default = \code{NULL}).
#' @param col.tips.nodes \code{character}, a vector of up to four colors to be
#'                       used for displaying respectively the tips, the nodes,
#'                       and (if \code{counts != 0}) the activated/counted nodes
#'                       and the nodes at which regions are counted.
#' @param counts \code{numeric}, whether to display the activations (\code{1}) or/and the homoplasies (\code{2}) or nothing (\code{0}; default).
#' @param use.edge.length \code{logical} indicating whether to use the edge lengths of the phylogeny to draw the branches or not (default).
#' @param col.states \code{logical}, whether to colour the states of the tips (\code{TRUE}) or not (\code{FALSE}, default).
#' @param state.labels vector of mode \code{character} containing labels for
#'                     each state of the character, in order, to be plotted if
#'                     col.states is \code{TRUE}.
#' @param legend.pos \code{character}, where to position the legend -- e.g. `bottomleft`.
#'                   Sent as `x` parameter to \code{\link{legend}}.
#'                   Specify `none` to hide the legend.`
#' @param y.lim \code{numeric} _x_ and _y_ coordinates for limits of the plot,
#'                    calculated automatically based on presence of legend if set to `NULL`
#'                    (the default).
#' @param \dots any optional arguments to be passed to \code{\link[ape]{plot.phylo}}
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
#' @author Thomas Guillerme, Martin R. Smith
#' @importFrom utils data
#' @export

plot.states.matrix <- function(
  x, passes = c(1,2,3,4), show.labels = 0,
  col.tips.nodes = c("#fc8d59", "#eeeeeed0", "#7fbf7be0", "#af8dc3e0"),
  counts = 0, use.edge.length = FALSE,
  col.states = FALSE, state.labels=character(0),
  legend.pos='bottomleft', y.lim=NULL, ...) {

    states_matrix <- x
    tree <- states_matrix$tree
    regions <- states_matrix$regions
    changes <- states_matrix$changes
    n_tip <- states_matrix$n_tip

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

        ## Convert into character
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
    if(!(class(passes) %in% c("numeric", "integer"))
       || any(is.na(match(passes, c(1,2,3,4))))) {
        if (length(passes) > 1 || !(passes %in% c(0, NULL, NA))) {
            warning("passes argument must be NULL, or any integer(s) between 1 and 4.")
        }
        passes = integer(0)
    }

    ## show.labels
    if(!is.null(show.labels)) {
        if(class(show.labels) != "numeric") {
            stop("show.labels argument must be either 1 for tips, 2 for nodes or c(1,2) for both")
        }
        ## Setting up the labels options
        show.tip.label <- any(1 %in% show.labels)
        show.node.label <- any(2 %in% show.labels)
    } else {
        show.tip.label <- show.node.label <- FALSE
    }

    ## col.tips.nodes
    if(length(col.tips.nodes) == 1) {
        col.tips.nodes <- rep(col.tips.nodes, 4)
    } else {
        if(length(col.tips.nodes) > 4) {
            col.tips.nodes <- col.tips.nodes[1:4]
            warning("Only the first four colors from col.tips.nodes are used.")
        }
    }

    ## counts
    if(any(counts != 0) && !(counts %in% c(1,2))) {
        stop("counts argument must be either 1 for displaying activations and/or 2 for homoplasies.")
    }


    ## Get the text plotting size
    cex <- 1

    ## Initialize the edges' colors
    edge_col <- "black"

    ## Read the tip states
    tips_labels <- plot.convert.state(states_matrix[[1]][1:n_tip], missing = TRUE)

    if (col.states) {
        ## Matching the states and colours
        tips_colours <- tips_labels
        tips_colours[nchar(tips_labels) > 1] <- "?"

        ## Select the palette
        max_colour <- max(as.integer(tips_colours[tips_colours %in% 0:9]))
        state_colours <- c(Inapp::brewer[[max_colour + 1]], "grey")
        names(state_colours) <- c(0:max_colour, "?")
        if ('-' %in% tips_labels) state_colours <- c(state_colours, '-' = 'lightgrey')


        ## Get the edge palette
        edge_palette <- state_colours
        edge_palette["?"] <- "darkgrey"
    }

    if(any(counts == 1) && !is.null(unlist(states_matrix$Up2))) {
      ## Change the colors of the edges' if activations exist (and if the algorithm is NA)
      edge_col <- ifelse(get.NA.edges(states_matrix, tree, pass = 4) == 1, "black", "grey")
    }

    ## Colour the states if the relevant uppass is available
    if (col.states && !is.null(unlist(states_matrix$Up1))) {
        ## get the states
        if (!is.null(unlist(states_matrix$Up2))) {
            final_state <- states_matrix$Up2
        } else {
            final_state <- states_matrix$Up1
        }
        all_states <- -1:max_colour
        col_states <- c('-', 0:max_colour)
        ## Get the edge colours
        colour.edge <- function (edge) {
            parent <- all_states %in% final_state[[edge[1]]]
            child <-all_states %in% final_state[[edge[2]]]
            common <- parent & child
            if (sum(common) == 1) {
                col_states[common]
            } else if (sum(child) == 1) {
                col_states[child]
            } else if (sum(parent) == 1) {
                col_states[parent]
            } else '?'
        }
        edge_final <- apply(tree$edge, 1, colour.edge)
        edge_col <- as.character(edge_palette[edge_final])
    }

    ## Plotting the tree
    if (is.null(y.lim)) y.lim <- c(if(legend.pos=='none' && length(state.labels) == 0) 0 else -3,
                                   n_tip+0.3)
    tree$tip.label <- paste("_", tree$tip.label) # Prefix with space to avoid the tiplabels() boxes
    graphics::plot(tree, show.tip.label = show.tip.label, type = "phylogram",
                   use.edge.length = use.edge.length, cex = cex,
                   adj = 0, edge.color = edge_col, edge.width = 2,
                   y.lim=y.lim,
                   ...)
    # plot(tree, show.tip.label = show.tip.label, type = "phylogram", use.edge.length = FALSE, cex = cex, adj = 0.5, edge.color = edge_col,  edge.width = 2) ; warning("DEBUG plot")

    if (legend.pos != "none") {
        ## Setting up the legend parameters
        length_text <-  paste("Tree score is", score.from(regions) + score.from(changes))
        if(all(counts == 0)) {
            legend_text <- length_text
            par_cex = 0
            par_pch = 0
            par_lty = 0
            par_lwd = 0
            par_col = "white"
        } else {
            if(all(counts == 1)) {
                legend_text <- c(length_text,
                                 paste("applicable region (1 + ",
                                       score.from(regions), ")", sep = ""),
                                 paste("additional region (",
                                       score.from(regions), ")", sep = ""))
                par_cex = c(0, 0, 2)
                par_pch = c(0, 0, 15)
                par_lty = c(0, 1, 0)
                par_lwd = c(0, 2, 0)
                par_col = c("white", "black", col.tips.nodes[4])
            } else {
                if(all(counts == 2)) {
                    legend_text <- c(length_text, paste("state changes (", score.from(changes), ")", sep = ""))
                    par_cex = c(0, 2)
                    par_pch = c(0, 15)
                    par_lty = c(0, 0)
                    par_lwd = c(0, 0)
                    par_col = c("white", col.tips.nodes[3])
                } else {
                    if(all(counts %in% c(1, 2))) {
                        legend_text <- c(length_text,
                                         paste("applicable region (1 + ", score.from(regions), ")", sep = ""),
                                         paste("additional region (", score.from(regions), ")", sep = ""),
                                         paste("state changes (", score.from(changes), ")", sep = ""))
                        par_cex = c(0, 0, 2, 2)
                        par_pch = c(0, 0, 15, 15)
                        par_lty = c(0, 1, 0, 0)
                        par_lwd = c(0, 2, 0, 0)
                        par_col = c("white", "black", col.tips.nodes[4], col.tips.nodes[3])
                    }
                }
            }
        }


        ## Adding the legend
        graphics::legend(legend.pos, legend = legend_text, cex = 1.2, pch = par_pch,
                         lty = par_lty, lwd = par_lwd, col = par_col,
                         pt.cex = par_cex, x.intersp = 0.5,
                         bty='n', bg = NULL)
    }
    ## Colour the tip states.
    if(col.states) {
        ape::tiplabels(tips_labels, cex = 1, adj = 1,
                       bg = paste0(state_colours[tips_colours])) #, 'aa'
        if (length(state.labels) == 0) {
            state_labels <- names(edge_palette)
        } else {
            if (length(state.labels) == length(edge_palette) - 2) {
                state.labels <- c(state.labels, 'Ambiguous', 'Inapplicable')
            } else if (length(state.labels) == length(edge_palette) - 1) {
                state.labels <- c(state.labels, 'Ambiguous')
            }
            state_labels <- paste(names(edge_palette), gsub("^['\"]|['\"]$", "", state.labels), sep=": ")
        }
        observed <- names(edge_palette) %in% edge_final
        graphics::legend('bottomright', legend=state_labels[observed], cex=1.2,
                         col=edge_palette[observed], x.intersp=1,
                         pch=15, pt.cex=1, lty=1, lwd=2,
                         bty='n', bg=NULL)
    } else {
      ape::tiplabels(tips_labels, cex = 1, bg = col.tips.nodes[1], adj = 1)
    }


    ## ADD THE NODE LABELS

    if(length(passes) > 0) {

        ## Get the first set of node labels
        node_labels <- plot.convert.state(states_matrix[[passes[1]+1]][-seq_len(n_tip)])
        node_labels <- paste(paste(passes[1], ":", sep = ""), node_labels)

        ## Adding node numbers (optional)
        if(show.node.label) {
            node_labels <- paste(paste("n",(n_tip+1):(n_tip + states_matrix$n_node), "\n", sep = ""), node_labels, sep = "")
        }

        ## Add the extra node labels
        for(pass in passes[-1]) {
            node_labels <- paste(node_labels, paste(pass, ": ", plot.convert.state(states_matrix[[pass + 1]][-seq_len(n_tip)]), sep = ""), sep = "\n")
        }

        ## Set the nodes colours
        bg_col <- rep(col.tips.nodes[2], states_matrix$n_node)

        if(all(counts == 1)) {
            ## Regions only
            if(length(regions) > 0) bg_col[regions - n_tip] <- col.tips.nodes[4]
        } else {
            if(all(counts == 2)) {
                ## Changes only
                if(length(changes) > 0) bg_col[changes - n_tip] <- col.tips.nodes[3]
            } else {
                if(all(counts %in% c(1,2))) {
                    ## Regions and changes
                    if(length(changes) > 0) bg_col[changes - n_tip] <- col.tips.nodes[3]
                    if(length(regions) > 0) bg_col[regions - n_tip] <- col.tips.nodes[4]
                    ## Overlapping
                    # if(any(changes %in% regions)) {
                    #     bg_col[changes[which(changes %in% regions)]] <-
                    # }
                }
            }
        }

        ## Plot the node labels
        ape::nodelabels(node_labels, cex = 0.90, bg = bg_col)
        # ape::nodelabels(node_labels, cex = 0.5, bg = bg_col) ; warning("DEBUG")
    }

    return(invisible())
}
