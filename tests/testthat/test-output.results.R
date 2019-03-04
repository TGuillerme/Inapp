context("Right output management")


test_that("make.output.data.frame works", {

    ## Generating some data
    tree <- ape::read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    character <- "23--1??--032"
    NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
    Fitch_matrix <- apply.reconstruction(tree, character, passes = 2, method = "Fitch")

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
    NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
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



test_that("write.nexus.commented works", {

    ## Check writing the notes
    set.seed(1)
    tree <- ape::rtree(5, br = NULL)
    node_notes <- lapply(as.list(seq(1:9)), function(X){paste("[", X, "]", sep = "")})
    out <- capture.output(write.nexus.commented(tree, comments = node_notes, file = ""))
    
    expect_equal(out[-2], c(
        "#NEXUS",
        "[More for at https://github.com/TGuillerme/Inapp/]",
        "",
        "BEGIN TAXA;",
        "\tDIMENSIONS NTAX = 5;",
        "\tTAXLABELS",
        "\t\tt3",
        "\t\tt4",
        "\t\tt1",
        "\t\tt2",
        "\t\tt5",
        "\t;",
        "END;",
        "BEGIN TREES;",
        "\tTRANSLATE",
        "\t\t1\tt3,",
        "\t\t2\tt4,",
        "\t\t3\tt1,",
        "\t\t4\tt2,",
        "\t\t5\tt5",
        "\t;",
        "\tTREE * UNTITLED = [&R] ((1[1],2[2])[7],(3[3],(4[4],5[5])[9])[8])[6];",
        "END;"
        ))

})



test_that("read.key work", {
    expect_message(read.key("a", "b", scan = FALSE))
    # expect_message(read.key("a", "b", scan = TRUE))
})


test_that("convert.binary.value work", {
    ## A random 5 taxa tree
    set.seed(1)
    tree <- ape::rtree(5, br = NULL)
    ## A character with inapplicable data
    character <- "01-?1"
    ## NA algorithm
    states_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")
    expect_equal(unlist(convert.binary.value(states_matrix[[1]], states_matrix)), c(2, 4, 1, 7, 4, 0, 0, 0, 0))

    ## Individual conversions work
    ## NA = 0
    expect_equal(unlist(convert.binary.value(list(NULL), states_matrix)), 0)
    ## -1 = 1
    expect_equal(unlist(convert.binary.value(list(-1), states_matrix)), 1)
    ## 0 = 2
    expect_equal(unlist(convert.binary.value(list(0), states_matrix)), 2)
    ## c(-1,0) = 3
    expect_equal(unlist(convert.binary.value(list(c(-1, 0)), states_matrix)), 3)
    ## 1 = 4
    expect_equal(unlist(convert.binary.value(list(1), states_matrix)), 4)
    ## c(-1, 1) = 5
    expect_equal(unlist(convert.binary.value(list(c(-1,1)), states_matrix)), 5)
    ## c(0, 1) = 6
    expect_equal(unlist(convert.binary.value(list(c(0,1)), states_matrix)), 6)
    ## c(-1, 0, 1) = 7
    expect_equal(unlist(convert.binary.value(list(c(-1,0,1)), states_matrix)), 7)

})


test_that("get.missing work", {
    all_states <- c(1,2)
    expect_equal(get.missing(1, all_states), 1)
    expect_equal(get.missing(2, all_states), 2)
    expect_equal(get.missing(c(1,2), all_states), "?")

})


test_that("output.results works properly", {
    ## A random 5 taxa tree
    set.seed(1)
    tree <- ape::rtree(5, br = NULL)
    ## A character with inapplicable data
    character <- "01-?1"
    ## NA algorithm
    NA_matrix <- apply.reconstruction(tree, character, passes = 4, method = "NA")


    ## Errors
    expect_error(output.states.matrix("NA_matrix", output = NULL, file = "Inapp_reconstruction", path = "."))
    expect_error(output.states.matrix(NA_matrix, output = "NULL", file = "Inapp_reconstruction", path = "."))
    expect_error(output.states.matrix(NA_matrix, output = 1, file = "Inapp_reconstruction", path = "."))
    expect_error(output.states.matrix(NA_matrix, output = c("pdf", "csv"), file = "Inapp_reconstruction", path = "."))
    expect_error(output.states.matrix(NA_matrix, output = "csv", file = c("a", "b"), path = "."))
    expect_error(output.states.matrix(NA_matrix, output = "csv", file = "Inapp_reconstruction", path = c("a", "b")))


    ## NULL return
    expect_null(output.states.matrix(NA_matrix, output = NULL, file = "Inapp_reconstruction", path = "."))


    ## Exporting the results as an annotated newick tree
    expect_null(output.states.matrix(NA_matrix, output = "newick"))
    ## Exporting the results and notes in a nexus file
    expect_null(output.states.matrix(NA_matrix, output = "nexus"))
    ## Exporting the result in a csv
    expect_null(output.states.matrix(NA_matrix, output = "csv"))
    ## Exporting the plot as a pdf
    expect_null(output.states.matrix(NA_matrix, output = "pdf"))
    ## Exporting the plot as C-test
    expect_null(output.states.matrix(NA_matrix, output = "C-test"))

    error_catch <- try(file.remove(list.files(pattern = "Inapp_reconstruction")))
})
