#' SVG canvas
#'
#' @param trees A list of trees, perhaps of class multiPhylo
#' @template outgroupTipsParam
#' @param analysisNames Character vector specifying name of each analysis, or
#' `character(0)` if not to be shown.
#' @param width,height Numeric specifying the size of the plot in pixels
#' @param xMargins,yMargins Numeric vectors of length 2 specifying the margin in pixels
#'  to be left at each side of the plot
#'
#' @return An object of class SVGCanvas
#' @exportClass SVGCanvas
#' @importFrom phangorn Ancestors Children
#' @importFrom TreeSearch SplitFrequency
#'
#' @author Martin R. Smith
#'
SVGCanvas <- function (trees, outgroupTips, analysisNames=character(0),
                       width = 682, height = 682,
                       xMargins=c(10L, 10L), yMargins=c(10L, 10L)) {
    uniqueTrees <- unique(trees)
    rootedTrees <- lapply(uniqueTrees, RootTree, outgroupTips=outgroupTips)
    nTree <- length(uniqueTrees)
    eachTree <- seq_len(nTree)
    treeIndex <- vapply(trees,
                     function (tr1)
                       which(vapply(uniqueTrees, all.equal, logical(1), tr1,
                                    use.tip.label=TRUE, use.edge.length=FALSE)),
                     integer(1))
    nTip <- length(uniqueTrees[[1]]$tip.label)
    nNode <- nTip - 1L
    internal <- nTip + seq_len(nNode)
    terminal <- seq_len(nTip)
    tipLabel <- matrix('', nTree, nTip)
    nEdge <- dim(tree$edge)[1]
    eachEdge <- seq_len(nEdge)
    nodeSupport <- matrix(0, nTree, nNode - 2L) # Two root nodes irrelevant
    parents <- children <- matrix(0, nTree, nEdge)
    xStep <- nodeX <- nodeY <- vector('list', nTree)
    yStep <- round((height - yMargins[2] - yMargins[1]) / nTip, 1)
    textY <- 3L
    nAnalyses <- length(analysisNames)
    for (i in eachTree) {
        tree <- rootedTrees[[i]]
        tipLabel[i, ] <- tree$tip.label
        treeEdge <- tree$edge
        parents[i, ] <- treeEdge[, 1]
        children[i, ] <- treeEdge[, 2]
        ancestors <- Ancestors(tree, type='all')
        nAncestors <- vapply(ancestors, length, 1)
        xStep[[i]] <- round(seq(xMargins[1], to=width-xMargins[2],
                                len=max(nAncestors) + 1L), 1)
        nodeX[[i]] <- xStep[[i]][nAncestors + 1L]
        tmpNodeY <- double(length(nAncestors))
        tmpNodeY[children[i, children[i, ] <= nTip]] <- seq(yMargins[1], height-yMargins[2], len=nTip)

        for (nodeI in rev(internal)) {
            tmpNodeY[nodeI] <- mean(tmpNodeY[Children(tree, nodeI)])
        }
        nodeY[[i]] <- round(tmpNodeY, 1)
        nodeSupport[i, ] <- round(SplitFrequency(tree, trees) / length(allTrees), 2)
    }
    structure( # For a S3 object
        list(height=height, width=width,
             rootedTrees = rootedTrees,
             analysisNames = analysisNames,
             nTree = nTree, eachTree = eachTree, treeIndex=treeIndex,
             nodeX = nodeX, nodeY = nodeY,
             nodeSupport = nodeSupport),
        class = 'SVGCanvas')
}

#' @export
is.SVGCanvas <- function (x) inherits(x, 'SVGCanvas')
#' @export
length.SVGCanvas <- function (x) x$nTree

#' SVG Tree
#'
#' @param treeNo Integer specifying which of the trees on the SVGForest to plot
#' @template canvasParam
#' @template treeNamesParam
#' @param char Character string specifying character to optimise on the tree
#' @param stateLabels Character vector specifying label for each applicable state
#' of the character
#' @param analysisLabels Character vector specifying label for each analysis
#'
#' @return A string describing an SVG object that depicts the tree, which can be
#' written to file or included in markdown destined for HTML output.
#' @export
#'
#' @author Martin R. Smith
SVGTree <- function (treeNo, canvas, char, stateLabels,
                     treeNames, analysisLabels=character(0)) {
    tree <- canvas$rootedTree[[treeNo]]
    nodeX <- canvas$nodeX[[treeNo]]
    nodeY <- canvas$nodeY[[treeNo]]
    nodeSupport <- canvas$nodeSupport[treeNo, ]
    statesMatrix <- apply.reconstruction(tree, char, match.tip.char=TRUE)
    fitchStates <- apply.reconstruction(tree, char, method='Fitch',
                                        inapplicable=1, match.tip.char=TRUE)
    matrixData <- MatrixData(statesMatrix, fitchStates, state.labels=stateLabels)
    legendLabels <- matrixData$legend
    legendCol <- matrixData$legend_col
    edgeCol <- matrixData$edge_col
    flagCol <- matrixData$tips_colours
    tipFlag <- matrixData$tips_labels

    LineStyle <- function (x) ifelse(x == 'lightgrey', '" class="inapplicable', '')
    edges <- paste0(vapply(unique(edgeCol), function(colour) {
        theseEdges <- edgeCol == colour
        paste0('<path d="', paste0(
            'M', nodeX[parents[treeNo, theseEdges]],
            ',', nodeY[parents[treeNo, theseEdges]],
            'V', nodeY[children[treeNo, theseEdges]],
            'H', nodeX[children[treeNo, theseEdges]],
            collapse=''),
            '" stroke="', colour, LineStyle(colour), '"></path>')
    }, character(1)), collapse='')
    tips <- paste0('<text x="', (nodeX[terminal] + 0L),
                   '" y="', nodeY[terminal] + textY,
                   '" fill="', flagCol[terminal],
                   '" class="flag">[', tipFlag[terminal], ']</text>',
                   '<text x="', (nodeX[terminal] + 22L),
                   '" y="', nodeY[terminal] + textY,
                   '" fill="', ifelse(flagCol == 'lightgrey', '#ccc',
                                      ifelse(flagCol == 'grey', '#999', '#000')),
                   '" class="taxonLabel">',
                   gsub('_', ' ', tipLabel[treeNo, terminal], fixed=TRUE), '</text>',
                   collapse='')
    nodes <- paste0('<text x="', (nodeX[internal][-1] + 2L),
                    '" y="', nodeY[internal][-1] + textY,
                    '" class="node" fill="', SupportColour(nodeSupport),
                    '">', nodeSupport, '</text>', collapse='')
    nRegions <- length(statesMatrix$regions)
    if (statesMatrix$score == fitchStates$score) {
        fitchNote1 <- fitchNote2 <- ''
    } else {
        fitchNote1 <- paste0(' (<tspan class="score fitch">', fitchStates$score,
                             '</tspan> with Fitch)')
        fitchNote2 <- ' (not counted by Fitch)'
    }
    ciCaption <- paste0('<text x="', canvas$width - 4L,
                        '" y="', yStep + textY, '" text-anchor="end" class="stepsLabel">',
                        'Character adds <tspan class="score">', statesMatrix$score,
                        '</tspan> to tree score', fitchNote1, '</text><text x="',
                        canvas$width - 4L, '" y="', (yStep * 2) + textY,
                        '" text-anchor="end" class="stepsLabel">',
                        '<tspan class="score">', nRegions,
                        '</tspan> additional region', ifelse(nRegions == 1, '', 's'),
                        fitchNote2, '</text><text x="', canvas$width - 4L,
                        '" y="', (yStep * 3) + textY, '" text-anchor="end" class="stepsLabel">',
                        # ci_text[i, treeNo],
                        '</text><text x="', canvas$width - 4L,
                        '" y="', (yStep * 5) + textY, '" text-anchor="end" class="stepsLabel">',
                        '<tspan dy="16" fill="#34caaf">An optimal tree under:</tspan>',
                        paste0('<tspan x="', canvas$width - 2L,
                               '" style="font-style:italic" class="',
                               ifelse(analysisLabels %in% treeNames[canvas$treeIndex==treeNo],
                                      'this', 'notThis'),
                               'An" dy="1.2em">', analysisLabels, '</tspan>', collapse=''),
                        '</text>')
    dudSteps <- matrixData$dud_steps

    svgSource <- paste0('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="',
                        canvas$width, '" height="', canvas$height, '" class="tree" data-char="', i, '">',
                        ciCaption, tips, edges, nodes,
                        '</svg>')
    # Return:
    svgSource
}

MatrixData <- function (states_matrix, fitch_states, state.labels) {
    tree <- states_matrix$tree
    regions <- states_matrix$regions
    changes <- states_matrix$changes
    steps <- fitch_states$changes
    dud_steps <- steps[!steps %in% changes]
    n_tip <- states_matrix$n_tip
    plot.convert.state <- function(char, missing = FALSE) {
        plot.convert.inappli <- function(X) {
            return(ifelse(X == -1, "-", X))
        }
        plot.convert.missing <- function(X, all_states) {
            if (length(all_states) > 1 && length(X) == length(all_states) &&
                all(sort(X) == sort(all_states))) {
                return("?")
            }
            else {
                return(X)
            }
        }
        if (missing) {
            all_states <- unique(unlist(char))
            char <- lapply(char, plot.convert.missing,
                                all_states)
        }
        char <- lapply(char, plot.convert.inappli)
        return(unlist(lapply(char, function(X) paste(as.character(X),
                                                          collapse = ""))))
    }
    get.NA.edges <- function(states_matrix, tree, pass = 4) {
        check.applicable <- function(nodes, states_matrix, pass) {
            node1 <- states_matrix[[pass + 1]][nodes[1]][[1]]
            node2 <- states_matrix[[pass + 1]][nodes[2]][[1]]
            all_char <- sort(unique(unlist(states_matrix$Char)))
            options(warn = -1)
            node2 <- ifelse(all(node2 == all_char), node1, node2)
            options(warn = 0)
            return(ifelse(all(c(node1, node2) == -1), 0, 1))
        }
        return(apply(tree$edge, 1, check.applicable, states_matrix,
                     pass))
    }

    edge_col <- "black"
    tips_labels <- plot.convert.state(states_matrix[[1]][1:n_tip], missing = TRUE)

    tips_colours <- tips_labels
    tips_colours[nchar(tips_labels) > 1] <- "?"
    max_colour <- max(as.integer(tips_colours[tips_colours %in%
                                                  0:9]))
    state_colours <- c(TreeSearch::brewer[[max_colour + 1]],
                       "grey")
    names(state_colours) <- c(0:max_colour, "?")
    if ("-" %in% tips_labels) state_colours <- c(state_colours, `-` = "lightgrey")
    edge_palette <- state_colours
    edge_palette["?"] <- "darkgrey"

    if (!is.null(unlist(states_matrix$Up2))) {
        na_edges <- get.NA.edges(states_matrix, tree, pass = 4) ==
            1
        edge_final <- ifelse(na_edges, "0", "-")
        edge_col <- ifelse(na_edges, "black", "grey")
    } else {
        edge_final = 0
    }
    #if (!is.null(unlist(states_matrix$Up1))) {
    if (!is.null(unlist(states_matrix$Up2))) {
        final_state <- states_matrix$Up2
    } else {
        final_state <- states_matrix$Up1
    }
    all_states <- -1:max_colour
    col_states <- c("-", 0:max_colour)
    colour.edge <- function(edge) {
        parent <- all_states %in% final_state[[edge[1]]]
        child <- all_states %in% final_state[[edge[2]]]
        common <- parent & child
        if (sum(common) == 1) {
            col_states[common]
        }
        else if (sum(child) == 1) {
            col_states[child]
        }
        else if (sum(parent) == 1 && !identical(parent,
                                                (col_states == "-"))) {
            col_states[parent]
        }
        else "?"
    }
    edge_final <- apply(tree$edge, 1, colour.edge)
    edge_col <- as.character(edge_palette[edge_final])
    #}

    if (length(state.labels) == length(edge_palette) - 2) {
        state.labels <- c(state.labels, "Ambiguous", "Inapplicable")
    } else if (length(state.labels) == length(edge_palette) - 1) {
        state.labels <- c(state.labels, "Ambiguous")
    } else if (length(state.labels) != length(edge_palette)) {
        warning("State labels do not seem to match states.  You need to label all states from 0 to the maximum observed.")
    }

    edge_col_array <- paste0('["', paste0(edge_col, collapse='", "'), '"]')

    state_labels <- paste(names(edge_palette), gsub("^['\"]|['\"]$",
                                                    "", state.labels), sep = ": ")
    observed <- names(edge_palette) %in% edge_final
    list (edge_col = edge_col,
          edge_col_array = edge_col_array,
          legend = state_labels[observed],
          legend_col = edge_palette[observed],
          tips_labels = tips_labels,
          tips_colours = as.character(state_colours[tips_colours]),
          dud_steps = dud_steps)
}

#' Plot character mapping
#'
#' Plot a tree with a character reconstruction plotted upon it
#'
#' @param char Character vector specifying distribution of tokens among (named) taxa
#' @param stateLabels Character vector specifying the labels to apply to each state
#' @param singleTree A single tree to be plotted in Latex output
#' @param legendText Character giving legend text to print in Latex output
#' @template canvasParam
#' @template treeNamesParam
#' @param analysisLabels Characte vector specifying names of each analysis, in order to be printed
#' @param svgFilename Character string specifying location to save each file,
#'   containing the expression \code{\%s}, which will be replaced with the number of the tree.
#' @param SetPar Graphical parameters to set before plotting PNG tree for Latex output.
#'
#' @return Prints the tree in an appropriate markdown format
#' @importFrom TreeSearch RootTree
#' @export
#' @author Martin R. Smith
PlotCharacterMapping <- function (char, stateLabels, singleTree, legendText,
                                  SetPar=par(mar=rep(0.2, 4), cex=0.7),
                                  canvas, treeNames,
                                  analysisLabels=canvas$analysisNames,
                                  svgFilename) {
    if (char[1] == '?' && length(unique(char)) == 1) {
        cat("<p>All taxa are coded as ambiguous for this character.</p>")
        legendLabels <- "?: Not scored"
        legendCol <- "darkgrey"
    } else if (!is.null(getOption('localInstance')) || knitr::is_html_output()) {
        for (treeNo in canvas$eachTree) {
            svgSource <- SVGTree(treeNo=treeNo, canvas=canvas,
                                 char=char, stateLabels=stateLabels, treeNames=treeNames,
                                 analysisLabels=analysisLabels)
            write(svgSource, file=sprintf(svgFilename, treeNo))
        }
        # Just write a single tree to HTML output
        cat(svgSource)
    } else {
        SetPar
        plot.states.matrix(apply.reconstruction(singleTree, char, match.tip.char=TRUE),
                           passes=0, counts=1:2, show.labels=1,
                           col.states=TRUE, state.labels=stateLabels,
                           use.edge.length=TRUE, legend.pos='topright')
        legend('bottomleft',  bty='n', legendText)
    }
}
