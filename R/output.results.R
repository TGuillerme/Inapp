#' @title Outputs results
#'
#' @description Outputs ancestral states reconstruction results
#'
#' @param states_matrix A \code{states.matrix} list from \code{\link{apply.reconstruction}}.
#' @param output The type of output, can be either \code{NULL} (default) or \code{"newick"}, \code{"nexus"}, \code{"csv"} or \code{"pdf"} (see details).
#' @param file The name of the output file(code{"character"}). 
#' @param path The path where to save the output file (code{"character"}).
#' @param ... If \code{output = "pdf"}, additional arguments to be passed to \code{plot.states.matrix}.
#'
#' @details
#' The output contains the for different reconstructions (Dp1, Up1, Dp2, Up2), the potential change or applicable region and the list of tips and nodes
#' Specifically, per data output type:
#' -\code{"newick"} outputs a newick string with annotated nodes as follows: (t1[Dp1:X,Up1:X,Dp2:X,Up2:X,Change:X,Applicable:X]).
#' -\code{"nexus"} outputs a nexus file with a newick string similar as above but also containing the total tree score, the matrix (one character) and the list of taxa.
#' -\code{"csv"} A matrix containing the different state reconstructions and score for each tip and node.
#' -\code{"pdf"} A pdf version of the states_matrix plotted with \code{plot.states.matrix}.
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
#' \dontrun{
#' ## Exporting the results as an annotated newick tree
#' output.states.matrix(states_matrix, output = "newick")
#'
#' ## Exporting the results and notes in a nexus file
#' output.states.matrix(states_matrix, output = "csv")
#'
#' ## Exporting the result in a csv
#' output.states.matrix(states_matrix, output = "csv")
#'
#' ## Exporting the plot as a pdf
#' output.states.matrix(states_matrix, output = "pdf")
#' }
#' 
#' @seealso \code{\link{apply.reconstruction}}, \code{\link{plot.states.matrix}}
#' 
#' @author Thomas Guillerme
#' @export

output.states.matrix <- function(states_matrix, output = NULL, file = "states_matrix", path = ".", ...) {

    match_call <- match.call()

    ## states_matrix
    if(class(states_matrix) != "states.matrix") {
        stop(paste(match_call$states_matrix, "must be of class 'states.matrix'.\nSee make.states.matrix() or apply.reconstruction()."))
    }

    ## output
    if(!is.null(output)) {
        ## Checking output's method
        all_outputs <- c("newick", "nexus", "csv", "pdf", "tre", "nex")
        if(class(output) != "character") {
            stop("output argument must be one of the following: ", paste(all_outputs[1:4], collapse = ", "), ".")
        } else {
            if(length(output) != 1) {
                stop("output argument must be one of the following: ", paste(all_outputs[1:4], collapse = ", "), ".")
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

    ## Filename and path
    if(class(file) != "character" || length(file) != 1) {
        stop(paste(match_call$file, "must be a single character string."))
    } else {
        if(class(path) != "character" || length(path) != 1) {
            stop(paste(match_call$path, "must be a single character string."))
        } else {
            full_path <- paste(path, paste(file, output, sep = "."), sep = "/")

            ## Check if the path exists
            if(!dir.exists(path)) {
                stop(paste("path", path, "not found."))
            }

            ## Check if file exists
            if(length(list.files(full_path)) != 0) {
                read.key(paste("file \"", full_path, "\" already exists!\nPress [enter] to overwrite or [esc] to cancel.", sep = ""), paste(file, "has been overwritten."))
            }
        }
    }


    ## Generating the output
    if(output %in% c("newick", "nexus")) {

        ## Create the data frame containing all nodes
        states_dataframe <- make.output.data.frame(states_matrix)

        ## Create the list of notes and ordering them to match 
        node_notes <- lapply(as.list(states_matrix$tree$edge[,2]), create.note, states_dataframe)

        ## Write the newick tree
        if(output == "newick") {
            write.tree.commented(tree, file = full_path, comments = node_notes, append = FALSE, digits = 10, tree.names = FALSE)
        }

        ## Write the nexus tree
        if(output == "nexus") {
            write.nexus.commented(tree, file = full_path, comments = node_notes, translate = TRUE)
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
        grDevices::pdf(file = full_path)
        plot.states.matrix(states_matrix, ...)
        # plot.states.matrix(states_matrix) ; warning("DEBUG output")
        dev.off()
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
# ' ## A balanced 12 taxa tree
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

# read.states.matrix <- function(file, path = ".") {
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



