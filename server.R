library(shiny)
library(ape)

## Load the R functions
source("helpers.R")

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
            tree <- ape::read.tree(text = input$newick_tree)
            if(is.null(tree)) {
              stop("Enter a tree in newick format.")
            }
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
            tree <- ape::read.tree(text = input$newick_tree)
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
                        write.csv(make.output.data.frame(states_matrix), file)
                    }
                    ## Save as a nexus
                    if(input$output_type == "nexus") {
                        write.csv(make.output.data.frame(states_matrix), file)
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