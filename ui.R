shinyUI(fluidPage(
  theme = 'slimline.css',

  wellPanel(

    titlePanel("Inapplicable data reconstruction"),
    p("Brazeau, M. D., Guillerme, T., and Smith, M. R. (2019). An algorithm for Morphological Phylogenetic Analysis with Inapplicable Data. Systematic Biology 68 (4), 619-631. doi:", a(href="https://dx.doi.org/10.1093/sysbio/syy083", "10.1093/sysbio/syy083"), "."),
    hr(),

    fluidRow(

        ## ---------------
        ## Tree parameters
        ## ---------------
        column(width = 4,
          ## Tree input - input$tree
          radioButtons("tree", label = h3("Tree input method"), choices = list("Random" = 1, "User (Newick)" = 2, "Nexus input" = 3), selected = 1),

          ## Random trees
          conditionalPanel(condition = "input.tree == 1",
            ## Type of tree - input$tree_type
            selectInput("tree_type", label = "Tree topology type", choices = list("Random", "Balanced", "Left", "Right", "Left-Right"), selected = "Random"),
            ## Number of taxa - input$n_taxa
            sliderInput("n_taxa", label = "Number of taxa:", min = 3, max = 100, value = 12),
            ## Messages for tree type :
            ## Help text for balanced trees
            conditionalPanel(condition = "input.tree_type == \"Balanced\"", helpText("For a balanced tree, the number of taxa must be a power of 2 otherwise a random tree is used instead.")),
            ## Help text for balanced trees
            conditionalPanel(condition = "input.tree_type == \"Left-Right\"", helpText("For a 'Left-Right' tree, the number of taxa must be even otherwise a random tree is used instead."))
          ),

          ## Newick trees
          conditionalPanel(condition = "input.tree == 2",
            ## Enter some newick string - input$newick_tree
            textInput("newick_tree", label = h5("Enter a newick tree:"), value = "((a,b),(c,d));"),
            ## Help text for the newick trees
            helpText("Make sure the number of opening and closing brackets match and that no commas (',') or the semi-colon (';') was omitted.")
          ),

          ## Nexus trees
          conditionalPanel(condition = "input.tree == 3",
            ## Upload some nexus tree
            fileInput("nexus_tree", label = h5("Select a newick or nexus format tree")),
            ## Tree number input - input$tree_num
            numericInput("tree_num", label = h5("Selected tree:"), value = 1, min = 1)
          ),

          ## Tips and nodes options
          checkboxGroupInput("showlabels", label = "Label:",
                             choices = list("Tips" = 1, "Nodes" = 2),
                             inline=TRUE, selected = NULL)
        ),

        ## --------------------
        ## Character parameters
        ## --------------------
        column(width = 4,
          ## Character input - input$character
          radioButtons("character", label = h3("Character input method"),
                       choices = list("Random" = 1, "User" = 2, "Nexus input" = 3),
                       selected = 1),

          ## Manual character input
          conditionalPanel(condition = "input.character == 2",
            ## Character string - input$character_string
            textInput("character_string", label = h5("Enter a character string:"), value = "0?1-"),
            ## Help text for the character string
            helpText("Accepted character states are any values from 0 to 9 as well ",
                     "as - for the inapplicable token and ? for all states (missing ",
                     "data). Polymorphic characters can be entered as {01}."),
            ## Match character
            checkboxInput("matchtipchar", label = "Match character to tips",
                          value = FALSE),
            helpText("Tick to match left-right state input order state to the ",
                     "alphanumeric order of the tip labels.")
          ),

          ## Nexus character
          conditionalPanel(condition = "input.character == 3",
            ## Nexus matrix input - input$nexus_matrix
            fileInput("nexus_matrix", label = h5("Select a nexus format matrix")),
            ## Character number input - input$character_num
            numericInput("character_num", label = h5("Selected character:"), value = 1, min = 1)
          ),

          ## Colour states
          checkboxInput("colour_states", label = "Colour states", value = TRUE)

        ),

        ## -------
        ## Display
        ## -------
        column(width = 4,
          ## Reconstruction - input$method
          radioButtons("method", label = h3("Reconstruction method"), choices = list("Inapplicable Fitch" = 1, "Normal Fitch" = 2), selected = 1, inline = TRUE),

          ## Inapplicable algorithm
          conditionalPanel(condition = "input.method == 1",
            ## Which passes to print
            checkboxGroupInput("showPassInapp", label = "Show passes:",
                               choices = list("1st Downpass" = 1, "1st Uppass" = 2,
                                              "2nd Downpass" = 3, "2nd Uppass" = 4),
                               selected = c(1,2,3,4))
          ),

          ## Fitch algorithm
          conditionalPanel(condition = "input.method == 2",
            ## How to treat missing data
            radioButtons("fitch_inapp", label = h5("Inapplicable tokens are:"), choices = list("Missing data (?)" = 1, "An extra state" = 2), selected = 1, inline = TRUE),
            helpText("When treated as ?, - is equal to all states; when treated as an extra state, - is equal to a new character state."),
            ## Which passes to show?
            checkboxGroupInput("showPassFitch", label = h5("Show passes"),  choices = list("1st Downpass" = 1, "1st Uppass" = 2), selected = c(1,2))
          ),

          ## Show activations/counts - input$counts
          checkboxGroupInput("counts", label = "Show counts of:",  choices = list("Applicable regions" = 1, "State changes" = 2), selected = c(1, 2)),

          hr(),
          ## Refresh button - input$refresh
          actionButton("refresh", label = "Refresh")

        )
    )
  ),

  ## Displaying the results
  fluidRow(
    ## Plots the algorithm results
    uiOutput("plot.ui")
  ),

  ## Plot configuration
  fluidRow(
    column(width = 4,
      sliderInput("plot_height", 'Output height', 20, 2000, 600, post = 'px')
    ),
    column(width = 4,
      sliderInput("font_size", "Text size", 0.4, 2, 1)
    ),
    column(width = 4,
      ## Export format - input$export_type
      selectInput("output_type", "Export results as:",
                  choices = c("csv", "pdf", "newick", "nexus", "C-test")),
      downloadButton('downloadData', 'Download')
    ),

    conditionalPanel(condition = "input.output_type == \"C-test\"",
                     ## Enter some traversal order - input$traversal_order
                     textInput("traversal_order", label = h5("Enter an optional traversal order for output:"), value = ""),
                     ## Help text for the traversal
                     helpText("The traversal must be written in a comma separated format (e.g. 1,2,3). If left as blank the order is: tips, root, right left nodes (traversal).")
    )

  )

))



