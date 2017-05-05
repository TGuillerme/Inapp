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
    expect_equal(dim(test_NA), c(23, 7))
    expect_equal(dim(test_Fitch), c(23, 4))

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

test_that("write.tree.commented works", {

    ## Generating some data
    tree <- ape::read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    character <- "23--1??--032"
    expect_warning(NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA"))
    test_NA <- make.output.data.frame(NA_matrix)


    ## Create the notes
    node_notes <- lapply(as.list(1:(ape::Ntip(NA_matrix$tree) + ape::Nnode(NA_matrix$tree))), create.note, test_NA)

    expect_is(node_notes, "list")
    expect_equal(length(node_notes), 23)
    expect_is(unlist(unique(lapply(node_notes, class))), "character")
    expect_equal(node_notes[[1]], "[Dp1=2,Up1=2,Dp2=2,Up2=2,Changes=FALSE,Regions=TRUE]")
    expect_equal(node_notes[[10]], "[Dp1=0,Up1=0,Dp2=0,Up2=0,Changes=FALSE,Regions=TRUE]")
    expect_equal(node_notes[[23]], "[Dp1=23,Up1=23,Dp2=23,Up2=023,Changes=TRUE,Regions=TRUE]")

    ## Check writing the notes
    set.seed(1)
    tree <- ape::rtree(5, br = NULL)
    node_notes <- lapply(as.list(seq(1:9)), function(X){paste("[", X, "]", sep = "")})
    newick <- write.tree.commented(tree, comments = node_notes, file = "")
    newick_decomp <- unlist(strsplit(strsplit(newick, split = ",")[[1]], split = ")"))

    expect_results <- unlist(node_notes)[c(1,2,7,3,4,5,9,8,6)]

    for(i in 1:length(newick_decomp)) {
        expect_equal(length(grep(expect_results[i], newick_decomp[i])), 1)
    }

})