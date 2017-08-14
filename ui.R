shinyUI(fluidPage(

  wellPanel(
  
    titlePanel("Inapplicable data reconstruction"),
    p("This will be supplementary material from", a(href="", "an awesome paper"), "."),
    hr(),
  
    fluidRow(
        ## ---------------
        ## Tree parameters
        ## ---------------
        column(width = 4,
          ## Tree input - input$tree
          radioButtons("tree", label = h3("Tree input method"), choices = list("Random" = 1, "User" = 2, "Nexus input" = 3), selected = 1),

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
            fileInput("nexus_tree", label = h5("Select a newick format tree"))
          ),

          ## Tips and nodes options
          checkboxGroupInput("showlabels", label = "Show labels", choices = list("Tips" = 1, "Nodes" = 2), selected = NULL)
        ),
        
        ## --------------------
        ## Character parameters
        ## --------------------
        column(width = 4,
          ## Character input - input$character
          radioButtons("character", label = h3("Character input method"), choices = list("Random" = 1, "User" = 2, "Nexus input" = 3), selected = 1),
          
          ## Manual character input
          conditionalPanel(condition = "input.character == 2",
            ## Character string - input$character_string
            textInput("character_string", label = h5("Enter a character string:"), value = "1?2-"),
            ## Help text for the character string
            helpText("Accepted character states are any values from 0 to 9 as well as - for the inapplicable token and ? for all states (missing data). Polymorphic characters can be entered as {01}."),
            ## Match character
            checkboxInput("matchtipchar", label = "Match character to tips", value = FALSE),
            helpText("Tick to match left-right state input order state to the alphanumeric order of the tip labels.")
          ),

          ## Nexus character
          conditionalPanel(condition = "input.character == 3",
            ## Nexus matrix input - input$nexus_matrix
            fileInput("nexus_matrix", label = h5("Select a newick format matrix")),
            ## Character number input - input$character_num
            numericInput("character_num", label = h5("Selected character:"), value = 1)
          ),

          ## --------------------
          ## Exports (in column "character")
          ## --------------------
          hr(),
          h3("Export results"),
          ## Export format - input$export_type
          selectInput("output_type", "Export format:", choices = c("csv", "pdf", "newick", "nexus", "C-test")),
          downloadButton('downloadData', 'Download')
          
          # hr(),
          # ## Citation! - input$cite_type
          # selectInput("cite_type", label = "Cite us", choices = list("format", "plain", "BibTeX"), selected = "format"),
          # ## Plain text format
          # conditionalPanel(condition = "input.cite_type == \"plain\"",
          #   helpText("Bob, Bib and Bub (2001) Something shiny here!.")
          # ),
          # ## BibTeX format
          # conditionalPanel(condition = "input.cite_type == \"BibTeX\"",
          #     helpText("@article{ShinyApp","    author={Bob, and Bib, and Bab},","    title={Something shiny},","    journal={Combinatorics},","    volume={1},","    number={1},","    pages={1:2},","    year={2001}}")
          # )
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
            checkboxGroupInput("showPassInapp", label = h5("Show passes"),  choices = list("1st Downpass" = 1, "1st Uppass" = 2, "2nd Downpass" = 3, "2nd Uppass" = 4), selected = c(1,2,3,4))
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
          checkboxGroupInput("counts", label = h5("Show counts details"),  choices = list("Applicable regions" = 1, "State changes" = 2), selected = 0),
        
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
  )

))



