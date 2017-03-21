shinyUI(fluidPage(

  # Create a row for additional information
  wellPanel(
    titlePanel("Inapplicable data reconstruction"),
    hr(),
    fluidRow(
        ## Tree input
        column(4,
          radioButtons("tree", label = h3("Tree input method"), choices = list("Random" = 1, "User" = 2, "Nexus input" = 3), selected = 1),
          ## Random tree
          conditionalPanel(
            condition = "input.tree == 1",
              selectInput("tree_type", label = "Tree topology type", choices = list("Random", "Balanced", "Left", "Right", "Left-Right"), selected = "Random"),
              sliderInput("n_taxa", label = "Number of taxa:", min = 3, max = 100, value = 12),
              helpText("If the tree type 'Balanced', the number of taxa must be a power of 2 (2,4,8, ...); if the tree type is 'Left-Right' the number of taxa must be even. In both cases, if the number of taxa does not match, a random tree is used instead.")
          ),
          ## Newick tree
          conditionalPanel(
            condition = "input.tree == 2",
              textInput("newick_tree", label = h5("Enter a newick tree:"), value = "((a,b),(c,d));"),
              helpText("Make sure the number of opening and closing brackets match and that no commas (',') or the semi-colon (';') was omitted.")
          ),
          ## Nexus tree
          conditionalPanel(
            condition = "input.tree == 3",
              fileInput("nexus_tree", label = h5("Select a newick format tree"))
          ),
          ## Tips and nodes
          checkboxGroupInput("showlabels", label = "Show labels", choices = list("Tips" = 1, "Nodes" = 2), selected = NULL)
        ),
        
        ## Character input
        column(4,
          radioButtons("character", label = h3("Character input method"), choices = list("Random" = 1, "User" = 2, "Nexus input" = 3), selected = 1),
          ## Input character
          conditionalPanel(
            condition = "input.character == 2",
              textInput("character_string", label = h5("Enter a character string:"), value = "1?2-"),
              helpText("The number of characters must match the size of the tree! Accepted states are any values from 0 to 9, - for the inapplicable token and ? for all states (missing data).")
          ),
          ## Nexus character
          conditionalPanel(
            condition = "input.character == 3",
              fileInput("nexus_matrix", label = h5("Select a newick format matrix")),
              numericInput("character_num", label = h5("Selected character:"), value = 1)
          ),

          ##
          hr(),
          h3("Export results"),
          actionButton("export_newick", label = "Newick"),
          actionButton("export_pdf", label = "pdf"),
          actionButton("export_xml", label = "xml"),
          hr(),
          selectInput("cite_type", label = "Cite us", choices = list("format", "plain", "BibTeX"), selected = "format"),
          conditionalPanel(
            condition = "input.cite_type == \"plain\"",
              helpText("Bob, Bib and Bub (2001) Something shiny here!.")
          ),
          conditionalPanel(
            condition = "input.cite_type == \"BibTeX\"",
              helpText("@article{ShinyApp,
                author={Bob, and Bib, and Bab},\n\ttitle={Something shiny},\n\tjournal={Combinatorics},\n\tvolume={1},\n\tnumber={1},\n\tpages={1:2},\nyear={2001}}")
          )
        ),

        ## Display
        column(4,
          ## Reconstruction
          radioButtons("method", label = h3("Reconstruction method"), choices = list("Inapplicable Fitch" = 1, "Normal Fitch" = 2), selected = 1, inline = TRUE),

          ## Inapplicable algorithm
          conditionalPanel(
            ## Which passes to show?
            condition = "input.method == 1",
              checkboxGroupInput("showPassInapp", label = h5("Show passes"),  choices = list("1st Downpass" = 1, "1st Uppass" = 2, "2nd Downpass" = 3, "2nd Uppass" = 4), selected = c(1,2,3,4))
          ),

          ## Fitch algorithm
          conditionalPanel(
            ## Which passes to show?
            condition = "input.method == 2",
              radioButtons("fitch_inapp", label = h5("Inapplicable tokens are:"), choices = list("Missing data (?)" = 1, "An extra state" = 2), selected = 1, inline = TRUE),
              helpText("When treated as ?, - is equal to all states; when treated as an extra state, - is equal to a new character state."),
              checkboxGroupInput("showPassFitch", label = h5("Show passes"),  choices = list("1st Downpass" = 1, "1st Uppass" = 2), selected = c(1,2))
          ),

          ## Show activations/counts
          checkboxGroupInput("counts", label = h5("Show counts"),  choices = list("Activations" = 1, "Homoplasies" = 2), selected = 3),
          
          ## Fitch inapplicable states
          # conditionalPanel(
          #   condition = "input.method == 2",
          #     radioButtons("fitch_inapp", label = h5("Inapplicable tokens are:"), choices = list("Missing data (?)" = 1, "An extra state" = 2), selected = 1),
          #     helpText("When treated as ?, - is equal to all states; when treated as an extra state, - is equal to a new character state.")
          # ),

          # conditionalPanel(
          #   condition = "any(input.counts == 1)",
          #     helpText("States activations are denoted with an asterisk.")
          # ),
          # conditionalPanel(
          #   condition = "any(input.counts == 2)",
          #     helpText("Homoplasies will be displayed somehow.")
          # ),
          hr(),
          actionButton("refresh", label = "Refresh")
          #actionButton("refresh", label = "Refresh"),

        )
    )
  ),
  


  fluidRow(
    ## Plots the algorithm results
    # size <- uiOutput("plot_size"),
    # textOutput("plot_size"),
    uiOutput("plot.ui")
    # plotOutput("plot_out")  ## Dynamically change the value here!
  )

))

    # ## DEBUG
    # mainPanel(
    #   plotOutput("plot_out", width = "100%"),
    #   textOutput("tree"),
    #   textOutput("character"),
    #   textOutput("character_string"),
    #   textOutput("tree_type"),
    #   textOutput("n_taxa"),
    #   textOutput("newick_tree"),
    #   textOutput("showPass")
    # )