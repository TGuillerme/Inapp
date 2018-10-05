#' @title Outputs results
#'
#' @description Outputs ancestral states reconstruction results
#'
#' @param states_matrix A \code{states.matrix} list from \code{\link{apply.reconstruction}}.
#' @param output The type of output, can be either \code{NULL} (default) or \code{"newick"}, \code{"nexus"}, \code{"csv"}, \code{"pdf"} or \code{"C-test"} (see details).
#' @param file The name of the output file(code{"character"}).
#' @param path The path where to save the output file (code{"character"}).
#' @param ... If \code{output = "pdf"}, additional arguments to be passed to \code{plot.states.matrix}.
#'
#' @details
#' The output contains the for different reconstructions (Dp1, Up1, Dp2, Up2), the potential change or applicable region and the list of tips and nodes
#' Specifically, per data output type:
#' -\code{"newick"} outputs a newick string with annotated nodes as follows: (t1\[Dp1:X,Up1:X,Dp2:X,Up2:X,Change:X,Applicable:X\]).
#' -\code{"nexus"} outputs a nexus file with a newick string similar as above but also containing the total tree score, the matrix (one character) and the list of taxa.
#' -\code{"csv"} A matrix containing the different state reconstructions and score for each tip and node.
#' -\code{"pdf"} A pdf version of the states_matrix plotted with \code{plot.states.matrix}.
#' -\code{"C-test"} a text version that can be used for testing in C (for DEBUG).
#'
#' @examples
#' ## A random 5 taxa tree
#' set.seed(1)
#' tree <- ape::rtree(5, br = NULL)
#' ## A character with inapplicable data
#' character <- "01-?1"
#'
#' ## NA algorithm
#' NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
#'
#' \dontrun{
#' ## Exporting the results as an annotated newick tree
#' output.states.matrix(NA_matrix, output = "newick")
#'
#' ## Exporting the results and notes in a nexus file
#' output.states.matrix(NA_matrix, output = "nexus")
#'
#' ## Exporting the result in a csv
#' output.states.matrix(NA_matrix, output = "csv")
#'
#' ## Exporting the plot as a pdf
#' output.states.matrix(NA_matrix, output = "pdf")
#' }
#'
#' @seealso \code{\link{apply.reconstruction}}, \code{\link{plot.states.matrix}}
#'
#' @author Thomas Guillerme
#' @export

output.states.matrix <- function(states_matrix, output = NULL, file = "Inapp_reconstruction", path = ".", ...) {

    match_call <- match.call()

    ## states_matrix
    if(class(states_matrix) != "states.matrix") {
        stop(paste(match_call$states_matrix, "must be of class 'states.matrix'.\nSee make.states.matrix() or apply.reconstruction()."))
    }

    ## output
    if(!is.null(output)) {
        ## Checking output's method
        all_outputs <- c("newick", "nexus", "csv", "pdf", "tre", "nex", "C-test")
        if(class(output) != "character") {
            stop("output argument must be one of the following: ", paste(all_outputs[1:4], collapse = ", "), ".")
        } else {
            if(length(output) != 1) {
                stop("output argument must be only one of the following: ", paste(all_outputs[1:4], collapse = ", "), ".")
            } else {
                if(!(output %in% all_outputs)) {
                    stop("output argument must be one of the following: ", paste(all_outputs[1:4], collapse = ", "), ".")
                }
            }
        }
    } else {
        ## If output is null, return.
        return(invisible())
    }
    ## Change output names
    output <- ifelse(output == "newick", "tre", output)
    output <- ifelse(output == "nexus", "nex", output)
    output <- ifelse(output == "C-test", "txt", output)

    ## Filename and path
    if(class(file) != "character" || length(file) != 1) {
        stop(paste(as.expression(match_call$file), "must be a single character string."))
    } else {
        if(class(path) != "character" || length(path) != 1) {
            stop(paste(as.expression(match_call$path), "must be a single character string."))
        } else {

            if(output != "txt") {
                ## Default output name
                full_path <- paste(path, paste(file, output, sep = "."), sep = "/")
            } else {
                ## C-test output name
                full_path <- paste(path, paste(file, "Ctest", output, sep = "."), sep = "/")
            }
            ## Check if the path exists
            if(!dir.exists(path)) {
                stop(paste("path", path, "not found."))
            }

            ## Check if file exists
            if(length(list.files(path = path, pattern = paste(file, output, sep = "."))) != 0) {
                read.key(paste("file \"", full_path, "\" already exists!\nPress [enter] to overwrite or [esc] to cancel.", sep = ""), paste(file, "has been overwritten."))
            }
        }
    }


    ## Generating the output
    if(output %in% c("tre", "nex")) {

        ## Create the data frame containing all nodes
        states_dataframe <- make.output.data.frame(states_matrix)

        ## Create the list of notes and ordering them to match
        # node_notes <- lapply(as.list(states_matrix$tree$edge[,2]), create.note, states_dataframe)
        node_notes <- lapply(as.list(1:(ape::Ntip(states_matrix$tree) + ape::Nnode(states_matrix$tree))), create.note, states_dataframe)

        ## Write the newick tree
        if(output == "tre") {
            write.tree.commented(states_matrix$tree, file = full_path, comments = node_notes, append = FALSE, digits = 10, tree.names = FALSE)
        }

        ## Write the nexus tree
        if(output == "nex") {
            write.nexus.commented(states_matrix$tree, file = full_path, comments = node_notes, translate = TRUE)
        }
        return(invisible())
    }

    if(output %in% "csv") {
        ## Output a nexus/newick file
        utils::write.table(make.output.data.frame(states_matrix), file = full_path, sep = ",")

        return(invisible())
    }

    if(output %in% "pdf") {
        ## Outputs a pdf
        # pdf_height <- Ntip(states_matrix$tree)
        # pdf_height <- ifelse(pdf_height < 7, 7, pdf_height)
        # pdf_height <- ifelse(pdf_height > 100, 100, pdf_height)
        # pdf_width <- ifelse(pdf_height == 7, 7, pdf_height * 0.75)
        pdf_width <- pdf_height <- 7
        grDevices::pdf(file = full_path, height = pdf_height, width = pdf_width)
        plot.states.matrix(states_matrix, ...)
        # plot.states.matrix(states_matrix) ; warning("DEBUG output")
        grDevices::dev.off()

        return(invisible())
    }

    if(output %in% "txt") {
        ## Outputs a C-test text file

        ## Setting the C variable name
        tree_var <- "char *test_tree"
        char_var <- "char *test_matrix"
        node_var <- "int node_pass"
        node_var <- paste0(node_var, 1:4, "[", ape::Ntip(states_matrix$tree)+ape::Nnode(states_matrix$tree), "] = ")

        ## Translate the tip labels
        if(!all(states_matrix$tree$tip.label == "numeric")) {
            if(length(grep("t", states_matrix$tree$tip.label)) == states_matrix$n_tip) {
                states_matrix$tree$tip.label <- gsub("t", "", states_matrix$tree$tip.label)
            } else {
                tstates_matrix$ree$tip.label <- seq(1:ape::Ntip(states_matrix$tree))
            }
        }

        ## Get the newick tree
        newick_tree_out <- paste0(tree_var, " = \"", ape::write.tree(states_matrix$tree), "\";")

        ## Get the matrix
        ## Get all the possible states (for ?)
        all_states <- sort(unique(unlist(states_matrix$Char)))
        ## Converts the missing data
        raw_matrix <- lapply(states_matrix$Char, get.missing, all_states)
        ## Collapse multiple states
        raw_matrix <- unlist(lapply(raw_matrix, paste, collapse = ""))
        ## Convert the NA
        raw_matrix <- gsub("-1", "-", raw_matrix)
        ## C output
        raw_matrix_out <- paste0(char_var, " = \"", paste(raw_matrix, collapse = ""), "\";")

        ## Get the node array
        node_values <- lapply(lapply(states_matrix[2:5], convert.binary.value, states_matrix), unlist)

        ## Get the right traversal order here
        traversal_order <- seq(from = 1, to = ape::Ntip(states_matrix$tree)+ape::Nnode(states_matrix$tree))

        ## Sort the passes by traversal
        passes_values <- list()
        for(pass in 1:4) {
            passes_values[[pass]] <- node_values[[pass]][traversal_order]
        }

        ## Get the node values in C format
        C_node_values <- lapply(passes_values, function(x) paste0("{", paste(x, collapse = ", "), "};"))
        C_node_values <- mapply(paste0, as.list(node_var), C_node_values)


        ## Combine both outputs
        txt_out <- c(raw_matrix_out, newick_tree_out, unlist(C_node_values))
        writeLines(txt_out, full_path)

        return(invisible())
    }

}

# ' @title Read files generated by \code{output.states.matrix}
# '
# ' @description Read files generated by \code{output.states.matrix}
# '
# ' @param file The path to the file generated by \code{output.states.matrix}.
# ' @param output The type of the file: \code{"newick"}, \code{"nexus"} or \code{"csv"}.
# '
# ' @return
# ' A \code{states.matrix} object.
# '
# ' @examples
# ' ## A balanced 12 taxon tree
# ' tree <- ape::read.tree(
# '                  text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
# ' ## A character with inapplicable data
# ' character <- "23--1??--032"
# '
# ' ## NA algorithm
# ' NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
# '
# ' @seealso \code{\link{apply.reconstruction}}, \code{\link{plot.states.matrix}}
# '
# ' @author Thomas Guillerme
# ' @export

#     ## Write nexus with node comments

#     ## Node data stucture:
#     #[Dp1:,Up1:,Dp2:,Up2:,Change,Applicable]

#     # ((a[Dp1:,Up1:,Dp2:,Up2:,Change,Applicable],b[])[], (c[],d[]));


#     ## Write output in xml

#     ## Write output in pdf


# ape::write.newick(rtree(5)) ## Tree with commented nodes

# ape::write.nexus(rtree(5)) ## Nexus file with matrix (1 character), and commented nodes

# utils::write.table(rtree(5)) ## A matrix with reconstructions

# grDevices::pdf(rtree(5)) ## A pdf of the tree

# }



