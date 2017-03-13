library(shiny)
library(ape)

# server.R
shinyServer(
    function(input, output, session) {

        ## Load the functions
        source("helpers.R")

        # ## Get the plot size out
        # output$plot_size <- renderUI({ 
        #     if(input$tree == 1) {
        #         ## Random tree
        #         tree <- ape::rtree(input$n_taxa)
        #     }

        #     ## Newick tree
        #     if(input$tree == 2) {
        #         tree <- ape::read.tree(text = input$newick_tree)
        #     }

        #     ## Nexus tree
        #     if(input$tree == 3) {
        #         nexus_tree <- input$nexus_tree
        #         if(!is.null(nexus_tree)) {
        #             tree <- ape::read.nexus(nexus_tree$datapath)
        #             ## Check if the tree is multiPhylo
        #             if(class(tree) == "multiPhylo") {
        #                 tree <- tree[[1]]
        #             }
        #         }
        #     }
        #     if(ape::Ntip(tree) > 10) {
        #         print(paste(round(ape::Ntip(tree)*0.4), "00px", sep = ""))
        #     } else {
        #         print(paste(4, "00px", sep = ""))
        #     }
        # })

        output$plot.ui <- renderUI({            
            ## Get the tree
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

            ## Set the plot window
            if(ape::Ntip(tree) > 10) {
                plotOutput("plot_out", width ="100%", height = paste(round(ape::Ntip(tree)*0.4), "00px", sep = ""))
            } else {
                plotOutput("plot_out", width ="100%", height = "400px")
            }
        })
    
        ## Generate the seeds for plotting
        seeds <- sample(1:200)*sample(1:10)

        output$plot_out <- renderPlot({ 
            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])

            ## ~~~~~~~~~~
            ## Tree
            ## ~~~~~~~~~~

            ## Setting up the tree
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

            ## ~~~~~~~~~~
            ## Character
            ## ~~~~~~~~~~

            ## Generate a random character
            if(input$character == 1) {
                character <- paste(sample(c("0", "1", "2", "-", "?"), ape::Ntip(tree), prob = c(0.535, 0.135, 0.13, 0.1, 0.1), replace = TRUE))
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

            ## ~~~~~~~~~~
            ## Plotting the results
            ## ~~~~~~~~~~

            # Inapplicable algorithm
            if(as.numeric(input$method) == 1) {
                plot.inapplicable.algorithm(tree, character, passes = as.vector(as.numeric(input$showPassInapp)), method = "Inapplicable", inapplicable = NULL, show.tip.label = input$showtiplabels)
            }

            ## Fitch algorithm
            if(as.numeric(input$method) == 2) {
                plot.inapplicable.algorithm(tree, character, passes = as.vector(as.numeric(input$showPassFitch)), method = "Fitch", inapplicable = as.numeric(input$fitch_inapp), show.tip.label = input$showtiplabels)
            }

        })#, height = 1200, width = 600)

        # # DEBUG:
        # output$tree <- renderText({ 
        #     if(input$tree == 1) {
        #         ## The input tree is random
        #         paste("Tree type:", input$tree_type, "with", input$n_taxa, "taxa")
        #     } else {
        #         ## The input tree is user based
        #         paste("Tree:", input$newick_tree, "with", Ntip(ape::read.tree(text = input$newick_tree)), "taxa")
        #     }
        # })

        # output$character <- renderText({ 
        #     if(input$character == 1) {
        #         paste("Character input: Random")
        #     } else {
        #         paste("Character input: ", input$character_string)
        #     }
        # })

        # output$character_string <- renderText({ 
        #     paste("character_string:", input$character_string)
        # })

        # output$tree_type <- renderText({ 
        #     paste("tree_type:", input$tree_type)
        # })

        # output$n_taxa <- renderText({ 
        #     paste("n_taxa:", input$n_taxa)
        # })

        # output$newick_tree <- renderText({ 
        #     paste("newick_tree:", input$newick_tree)
        # })

    }
)