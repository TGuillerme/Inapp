library(shiny)
library(ape)

## Load the R functions
source("helpers.R")

## Sanitise input text to check that newick tree can be extracted
read.newick.tree <- function (newick_text) {
    if (is.null(newick_text)) {
        stop("Enter a tree in newick format.")
    }

    newick_text <- trimws(newick_text)
    chars_to_count <- c("\\(", "\\)", ",")
    char_counts <- vapply(chars_to_count, function (char)
    lengths(regmatches(newick_text, gregexpr(char, newick_text))), 0)
    if (length(unique(char_counts)) > 1) {
        stop("Braces and commas in input tree must balance: ", char_counts[1], " (s; ", char_counts[2], " )s; ", char_counts[3], " commas.")
    }

    # Add trailing semicolon, if missing
    if (substr(newick_text, nchar(newick_text), nchar(newick_text)) != ";") {
        newick_text <- paste0(newick_text, ';')
    }

    return (ape::read.tree(text = newick_text))
}

## Get the tree details
## Return a tree, or an error message to be written to output.
get.tree <- function(input, session, simple = FALSE) {

    ## Wrapper for getting an input tree
    get.input.tree <- function(input, session) {
        nexus_tree <- input$nexus_tree
        if(!is.null(nexus_tree)) {
            ## Read the tree
            tree <- read.any.tree(nexus_tree$datapath)
            ## Check if the tree is multiPhylo
            if(class(tree) == "multiPhylo") {
                ## Update max tree selection
                shiny::updateNumericInput(session, "tree_num", max = length(tree))
                ## Select the tree
                tree <- tree[[input$tree_num]]
            }
            return(tree)
        } else {
            return("Load a tree in nexus or newick format.")
        }
    }

    # Initialize to avoid returning unset variable
    tree <- "No tree specified."

    if(!simple) {
        ## Get the not "simple" tree (for operations)
        if(input$tree == 1) {
            ## Balanced tree
            if(input$tree_type == "Balanced") {
                if(log2(input$n_taxa)%%1 == 0) {
                    tree <- ape::stree(input$n_taxa, type = "balanced")
                } else {
                    tree <- ape::rtree(input$n_taxa)
                }
            }

            ## Left-Right tree
            if(input$tree_type == "Left-Right") {
                if(input$n_taxa%%2 == 0) {
                    left <- ape::stree(input$n_taxa/2+1, type = "left")
                    right <- ape::stree(input$n_taxa/2, type = "right")
                    tree <- ape::bind.tree(left, right, where = 1)
                } else {
                    tree <- ape::rtree(input$n_taxa)
                }
            }

            ## Left tree
            if(input$tree_type == "Left") {
                tree <- ape::stree(input$n_taxa, type = "left")
            }

            ## Right tree
            if(input$tree_type == "Right") {
                tree <- ape::stree(input$n_taxa, type = "right")
            }

            ## Random tree
            if(input$tree_type == "Random") {
                tree <- ape::rtree(input$n_taxa)
            }
        }

        ## Newick tree
        if(input$tree == 2) {
            tree <- read.newick.tree(input$newick_tree)
        }

        ## Nexus tree
        if(input$tree == 3) {
            tree <- get.input.tree(input, session)
        }

    } else {
        ## Get the simplest tree (for tip count)
        if(input$tree == 1) {
            ## Random tree
            tree <- ape::rtree(input$n_taxa)
        }

        ## Newick tree
        if(input$tree == 2) {
            tree <- read.newick.tree(input$newick_tree)
        }

        ## Nexus tree
        if(input$tree == 3) {
            tree <- get.input.tree(input, session)
        }
    }

    return(tree)
}

#' Getting the character details
#' @param session Shiny session (to allow updating of character selection)
#' @return a character string if character extracted correctly,
#'  a list (detailing the error message to be displayed) if there's an error.
get.character <- function(input, tree, session) {
    n_tip <- ape::Ntip(tree)
    ## Generate a random character
    if(input$character == 1) {
        character <- paste(sample(c("0", "1", "2", "-", "?"), n_tip, prob = c(0.2, 0.2, 0.1, 0.15, 0.1), replace = TRUE))
        ## Adding at least three inapplicable tokens (if there's at least 5 tips)
        if(n_tip >= 5) {
            character[sample(1:length(character), 3)] <- "-"
        }
    }

    ## Character input as a character string
    if(input$character == 2) {
        character <- as.character(input$character_string)
        if(is.null(character)) {
            return(list("Enter the character as a string (e.g. 0123)."))
        }
    }

    ## Character input as a nexus
    if(input$character == 3) {
        nexus_matrix <- input$nexus_matrix
        if(!is.null(nexus_matrix)) {
            ## Select the right character
            if (is.na(input$character_num)) {
                return (list("Character selection must be numeric."))
            }

            character <- TreeTools::ReadCharacters(nexus_matrix$datapath, input$character_num, session = session)
            if (class(character) == 'list') return (character)
            matrix_taxa <- rownames(character)
            if (all(tree$tip.label %in% matrix_taxa)) {
              data_matrix <- character[tree$tip.label, ]
              #data_matrix <- vapply(tree$tip.label, function (tip) data_matrix[[tip]], data_matrix[[1]])
            } else {
              missingTaxa <- tree$tip.label[!tree$tip.label %in% matrix_taxa]
              return(list("Tree contains taxa [", paste(missingTaxa, collapse = ", "), "] not found in Nexus matrix."))
            }
        } else {
            return(list("Load a matrix in Nexus format."))
        }
    }
    return(character)
}

## Generate the seeds for plotting
seeds <- sample(1:200)*sample(1:10)


# server.R
shinyServer(
    function(input, output, session) {
        ## Plotting function
        output$plot_out <- renderPlot({
            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])


            ## Getting the parameters
            tree <- get.tree(input, session)
            if (class(tree) == 'character') {
              return(plotError(tree))
            } else if (class(tree) != 'phylo'){
              return(plotError("The tree must be of class 'phylo'."))
            }

            character <- get.character(input, tree, session)
            if (class(character) == 'list') {
              return(plotError(paste(character, sep = '', collapse = '')))
            }

            if (length(character) == 0) {
              return(plotError("Character of length zero."))
            }

            n_tip <- length(tree$tip.label)
            if (class(character) == 'matrix') {
                state_labels <- attr(character, 'state.labels')[[1]]
                # TODO! If unobserved states are labelled, they should be removed!

                character_name <- colnames(character)

                if (all(tree$tip.label %in% rownames(character))) {
                    character <- character[tree$tip.label, ]
                } else {
                    return(plotError(paste("No entries in character list correspond to ",
                                     paste(tree$tip.label[!(tree$tip.label %in% rownames(character))]))))
                }
            } else {
                character_name <- NULL
                state_labels <- NULL
            }

            ## Transform character
            if(class(character) != "list") {
                character <- convert.char(character)
            }

            ## Check if the character is the same length as the tree
            if (n_tip != length(character)) {
                return(plotError(paste(n_tip, " tips in the tree, but ",
                                       length(character), " entries in the data matrix")))
            }

            ## Run the algorithm
            if(as.numeric(input$method) == 1) {
                states_matrix <- apply.reconstruction(tree, character, method = "NA", inapplicable = NULL, match.tip.char = as.logical(input$matchtipchar))
            } else {
                states_matrix <- apply.reconstruction(tree, character, method = "Fitch", inapplicable = as.numeric(input$fitch_inapp), match.tip.char = as.logical(input$matchtipchar))
            }

            ## ~~~~~~~~~~
            ## Plotting the results
            ## ~~~~~~~~~~

            ## Graphical options
            if(is.null(input$showlabels)) {
                showlabels <- NULL
            } else {
                showlabels <- as.numeric(input$showlabels)
            }

            ## Passes
            if(as.numeric(input$method) == 1) {
                show_passes <- as.vector(as.numeric(input$showPassInapp))
            } else {
                show_passes <- as.vector(as.numeric(input$showPassFitch))
            }

            plot.states.matrix(states_matrix, passes = show_passes,
                               show.labels = showlabels,
                               counts = as.vector(as.numeric(input$counts)),
                               col.states = input$colour_states,
                               state.labels = state_labels)
            mtext(side=c(1, 3), character_name, font=2)

            ## Exporting data
            output$downloadData <- downloadHandler(

                ## Filename management
                filename = function() {
                    ## Managing the output suffix
                    suffix <- input$output_type
                    suffix <- ifelse(suffix == "newick", "tre", suffix)
                    suffix <- ifelse(suffix == "nexus", "nex", suffix)
                    suffix <- ifelse(suffix == "C-test", "txt", suffix)
                    ## Getting the output name
                    paste(paste("Inapp", format(Sys.time(), "%Y-%m-%d-%H%M%S"), sep = "_"), sep = ".", suffix)
                },

                ## Export management
                content = function(file) {
                    ## Save as a csv
                    if(input$output_type == "csv") {
                        write.csv(make.output.data.frame(states_matrix), file)
                    }
                    ## Save as a pdf
                    if(input$output_type == "pdf") {
                        pdf(file)
                        plot.states.matrix(states_matrix, passes = show_passes, show.labels = showlabels, counts = as.vector(as.numeric(input$counts)))
                        dev.off()
                    }
                    ## Save as a newick
                    if(input$output_type == "newick") {
                        tree$edge.length <- NULL
                        states_dataframe <- make.output.data.frame(states_matrix)
                        node_notes <- lapply(as.list(1:(states_matrix$n_tip + states_matrix$n_node)), create.note, states_dataframe)
                        write.tree.commented(tree, file, comments = node_notes, append = FALSE, digits = 10, tree.names = FALSE)
                    }
                    ## Save as a nexus
                    if(input$output_type == "nexus") {
                        tree$edge.length <- NULL
                        states_dataframe <- make.output.data.frame(states_matrix)
                        node_notes <- lapply(as.list(1:(states_matrix$n_tip + states_matrix$n_node)), create.note, states_dataframe)
                        write.nexus.commented(tree, file, comments = node_notes, translate = TRUE)
                    }
                    ## Save as a C-test
                    if(input$output_type == "C-test") {
                        tree$edge.length <- NULL

                        ## Setting the C variable name
                        tree_var <- "char *test_tree"
                        char_var <- "char *test_matrix"
                        node_var <- "int node_pass"
                        node_var <- paste0(node_var, 1:4, "[", states_matrix$n_tip + states_matrix$n_node, "] = ")

                        ## Translate the tip labels
                        if(!all(tree$tip.label == "numeric")) {
                            if(length(grep("t", tree$tip.label)) != 0) {
                                tree$tip.label <- gsub("t", "", tree$tip.label)
                            } else {
                                tree$tip.label <- seq(1:states_matrix$n_tip)
                            }
                        }

                        ## Get the newick tree
                        newick_tree_out <- paste0(tree_var, " = \"", ape::write.tree(tree), "\";")

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
                        if(input$traversal_order == "") {
                            traversal_order <- seq(from = 1, to = states_matrix$n_tip + states_matrix$n_node)
                        } else {
                            traversal_order <- as.numeric(unlist(strsplit(input$traversal_order, ",")))
                        }

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
                        writeLines(txt_out, file)
                    }
                }
            )
        })

        ## Output plot
        output$plot.ui <- renderUI({

            tree <- get.tree(input, session, simple = TRUE)
            if (class(tree) == "character") {
                plotOutput("plot_out", width ="100%", height = "40px")
                plotError(tree)
            } else {
                n_tip <- length(tree$tip.label)
                ## Set the plot window
                plotOutput("plot_out", width ="100%", height = paste(round((n_tip + 3.3) * 0.4) * 90L, "px", sep = ""))
            }
        })
    }
)
