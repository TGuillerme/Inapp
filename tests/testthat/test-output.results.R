context("Right output management")


test_that("make.output.data.frame works", {

    ## Generating some data
    tree <- ape::read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    character <- "23--1??--032"
    expect_warning(NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA"))
    expect_warning(Fitch_matrix <- apply.reconstruction(tree, character, passes = 2, method = "Fitch"))

    ## Creating the output matrix
    test_NA <- make.output.data.frame(NA_matrix)
    test_Fitch <- make.output.data.frame(Fitch_matrix)

    ## Right output type
    expect_is(test_NA, "data.frame")
    expect_is(test_Fitch, "data.frame")
    expect_equal(dim(test_NA), c(23, 6))
    expect_equal(dim(test_Fitch), c(23, 3))

    ## Right output values
    expect_is(test_NA$Dp1, "character")
    expect_is(test_NA$Up1, "character")
    expect_is(test_NA$Dp2, "character")
    expect_is(test_NA$Up2, "character")
    expect_is(test_NA$Changes, "logical")
    expect_is(test_NA$Regions, "logical")

    expect_is(test_Fitch$Dp1, "character")
    expect_is(test_Fitch$Up1, "character")
    expect_is(test_Fitch$Dp2, "NULL")
    expect_is(test_Fitch$Up2, "NULL")
    expect_is(test_Fitch$Changes, "logical")
    expect_is(test_Fitch$Regions, "NULL")
})