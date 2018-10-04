context("inapplicable.algorithm")
test_that("inapplicable.algorithm works", {
    

    ## Random tree with 12 taxa
    tree <- ape::rtree(12, br = NULL)
    ## A character with inapplicable data
    character <- "23--1??--032"
    ## Normal Fitch algorithm (NA states are missing data)
    expect_error(apply.reconstruction(tree, character, passes = 2, method = "Futch", inapplicable = 1))
    expect_error(apply.reconstruction(tree, character, passes = 2, method = "Fitch", inapplicable = 3))
})