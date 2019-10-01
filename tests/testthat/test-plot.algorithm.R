context("plot.algorithm")
library('vdiffr')
test_that("plot.algorithm works", {
    ## A balanced 12 taxon tree
    tree <- ape::read.tree(
                     text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    ## A character with inapplicable data
    character <- "23--1??--032"
    ## NA algorithm
    NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")

    ## Plotting the tree and the states
    plot_NA_matrix <- function () plot(NA_matrix)
    expect_doppelganger('Plot NA matrix', plot_NA_matrix)
    ## Plotting the tree and the states with the state changes and regions
    expect_warning(expect_null(plot(NA_matrix, counts = c(1, 2))))
    ## Plot the tree with tip/node labels, and only the 1st and 2nd downpass
    expect_null(plot(NA_matrix, passes = c(1, 3), show.labels = c(1, 2)))
    ## Plot the tree only the 2nd uppass with the state changes in green
    expect_warning(expect_null(plot(NA_matrix, show.labels = 2,
                                    col.tips.nodes = c("red", "pink", "green"),
                                    counts = c(1,2), passes = c(3,4))))

    no_applicables <- apply.reconstruction(tree, '??--??--??--',
                                           passes = 4, method = "NA")
    plot_no_applicables <- function () plot(no_applicables)
    expect_doppelganger('Plot no applicables', plot_no_applicables)

})

