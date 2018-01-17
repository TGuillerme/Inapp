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
  if (length(unique(vapply(chars_to_count, function (char)
     lengths(regmatches(newick_text, gregexpr(char, newick_text))), 0))) > 1) {
    stop("Braces and commas in input tree must balance.")
  }
  
  # Add trailing semicolon, if missing
  if (substr(newick_text, nchar(newick_text), nchar(newick_text)) != ";") {
    newick_text <- paste0(newick_text, ';')
  }

  return (ape::read.tree(text = newick_text))
}
  
## Get the tree details
get.tree <- function(input, simple = FALSE) {

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
            nexus_tree <- input$nexus_tree
            if(!is.null(nexus_tree)) {
                tree <- ape::read.nexus(nexus_tree$datapath)
                ## Check if the tree is multiPhylo
                if(class(tree) == "multiPhylo") {
                    tree <- tree[[1]]
                }
            } else {
                stop("Load a tree in nexus format.")
            }
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
            nexus_tree <- input$nexus_tree
            if(!is.null(nexus_tree)) {
                tree <- ape::read.nexus(nexus_tree$datapath)
                ## Check if the tree is multiPhylo
                if(class(tree) == "multiPhylo") {
                    tree <- tree[[1]]
                }
            }
        }

    }

    return(tree)
}

## Getting the character details
get.character <- function(input, tree) {
    ## Generate a random character
    if(input$character == 1) {
        character <- paste(sample(c("0", "1", "2", "-", "?"), ape::Ntip(tree), prob = c(0.2, 0.2, 0.1, 0.15, 0.1), replace = TRUE))
        ## Adding at least three inapplicable tokens (if there's at least 5 tips)
        if(ape::Ntip(tree) >= 5) {
            character[sample(1:length(character), 3)] <- "-"
        }
    }

    ## Character input as a character string
    if(input$character == 2) {
        character <- as.character(input$character_string)
        if(is.null(character)) {
            stop("Enter a character as a string (e.g. 0123).")
        }
    }

    ## Character input as a nexus
    if(input$character == 3) {
        nexus_matrix <- input$nexus_matrix
        if(!is.null(nexus_matrix)) {
            matrix <- ape::read.nexus.data(nexus_matrix$datapath)
            matrix <- matrix(data = unlist(matrix), nrow = length(matrix[[1]]), byrow = FALSE)
            ## Select the right character
            if(input$character_num < 1 | input$character_num > nrow(matrix)) {
                stop(paste("Select a character between 0 and ", nrow(matrix), ".", sep = ""))
            } else {
                character <- matrix[input$character_num, ]
            }
        } else {
            stop("Load a matrix in nexus format.")
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
            tree <- get.tree(input)
            character <- get.character(input, tree)

            ## Run the algorithm
            if(as.numeric(input$method) == 1) {
                states_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA", inapplicable = NULL, match.tip.char = as.logical(input$matchtipchar))
            } else {
                states_matrix <- apply.reconstruction(tree, character, passes = 2, method = "Fitch", inapplicable = as.numeric(input$fitch_inapp), match.tip.char = as.logical(input$matchtipchar))
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

            plot.states.matrix(states_matrix, passes = show_passes, show.labels = showlabels, counts = as.vector(as.numeric(input$counts)))


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
                    paste(paste("Inapp", format(Sys.time(), "%Y-%m-%d-%H%M%S"), sep = "_"), sep = ".", suffix)  #TG: or date format as "format(Sys.time(), "%Y-%m-%d-%X")"
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
                        node_notes <- lapply(as.list(1:(ape::Ntip(tree) + ape::Nnode(tree))), create.note, states_dataframe)
                        write.tree.commented(tree, file, comments = node_notes, append = FALSE, digits = 10, tree.names = FALSE)
                    }
                    ## Save as a nexus
                    if(input$output_type == "nexus") {
                        tree$edge.length <- NULL
                        states_dataframe <- make.output.data.frame(states_matrix)
                        node_notes <- lapply(as.list(1:(ape::Ntip(tree) + ape::Nnode(tree))), create.note, states_dataframe)
                        write.nexus.commented(tree, file, comments = node_notes, translate = TRUE)
                    }
                    ## Save as a C-test
                    if(input$output_type == "C-test") {
                        tree$edge.length <- NULL

                        ## Setting the C variable name
                        tree_var <- "char *test_tree"
                        char_var <- "char *test_matrix"
                        node_var <- "int node_pass"
                        node_var <- paste0(node_var, 1:4, "[", ape::Ntip(tree) + ape::Nnode(tree), "] = ")

                        ## Translate the tip labels
                        if(!all(tree$tip.label == "numeric")) {
                            if(length(grep("t", tree$tip.label)) != 0) {
                                tree$tip.label <- gsub("t", "", tree$tip.label)
                            } else {
                                tree$tip.label <- seq(1:ape::Ntip(tree))
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
                            traversal_order <- seq(from = 1, to = ape::Ntip(tree) + ape::Nnode(tree))
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

            tree <- get.tree(input, simple = TRUE)
            
            ## Set the plot window
            if(ape::Ntip(tree) > 10) {
                plotOutput("plot_out", width ="100%", height = paste(round(ape::Ntip(tree)*0.4), "00px", sep = ""))
            } else {
                plotOutput("plot_out", width ="100%", height = "400px")
            }
        })
    }
)