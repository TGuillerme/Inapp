source("../plot.algorithm.R")
library(testthat)
library(ape)

## convert.char
context("convert.char")
test_that("convert.char works", {
    character <- "01---?010101"
    list_out <- list(0,1,-1,-1,-1,c(0,1),0,1,0,1,0,1)

    ## Not working (return NULL)
    expect_null(convert.char(NULL))
    expect_null(convert.char(matrix(1)))
    expect_null(convert.char(data.frame(1)))

    ## Right output format (returns a list)
    expect_is(convert.char(1), "list")
    expect_is(convert.char("1"), "list")
    expect_is(convert.char(c(1)), "list")
    expect_is(convert.char(list(1)), "list")

    ## Right conversion
    expect_true(all(mapply(function(X,Y) {all(X == Y)}, convert.char(list_out), list_out)))
    expect_warning(test <- convert.char("01-?")) # Warning is some weird NA management by testthat
    expect_true(unlist(test[1]) == 0)
    expect_true(unlist(test[2]) == 1)
    expect_true(unlist(test[3]) == -1)
    expect_true(all(unlist(test[4]) == c(-1,0,1)))
})

context("make.states.matrix")
test_that("make.states.matrix works", {
    tree <- rtree(4)
    character <- "01-?"

    ## Not working (error)
    expect_error(make.states.matrix("tree", character, inapplicable = NULL))
    expect_error(make.states.matrix(tree, 1, inapplicable = NULL))

    ## Right output style
    expect_warning(matrix <- make.states.matrix(tree, character))  # Warning is some weird NA management by testthat
    expect_is(matrix, "list")
    expect_equal(unique(unlist(lapply(matrix, class))), c("list", "numeric"))
    expect_equal(unique(unlist(lapply(matrix, length))), c(7,4,1))
    expect_equal(length(matrix), 7)
    expect_equal(names(matrix), c("Char", "Dp1", "Up1", "Dp2", "Up2", "tracker", "length"))

    ## Right output values
    expect_warning(expect_equal(unlist(make.states.matrix(tree, character)[[1]]), c(0,1,-1,-1,0,1)))  # Warning is some weird NA management by testthat
    expect_warning(expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 1)[[1]]), c(0,1,0,1,0,1)))  # Warning is some weird NA management by testthat
    expect_warning(expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 2)[[1]]), c(0,1,2,0,1,2)))  # Warning is some weird NA management by testthat
})

context("desc.anc")
test_that("desc.anc works", {
    set.seed(1)
    tree <- rtree(4)

    ## Not working (wrong input)
    expect_error(desc.anc(1, "tree"))
    expect_error(desc.anc(1, matrix(0)))
    expect_equal(desc.anc("a", tree), numeric(0))
    expect_equal(desc.anc(88, tree), numeric(0))

    ## Right output
    expect_is(desc.anc(6, tree), "integer")

    ## Right answers
    answers <- list(c(5), c(6), c(7), c(7), c(1, 6), c(2, 7, 5), c(3, 4, 6))
    for(test in 1:7) {
        expect_equal(desc.anc(test, tree), answers[[test]])
    }
})

context("get.common")
test_that("get.common works", {
    ## Not working
    expect_error(get.common(matrix(1), tree))
    expect_error(get.common(1))

    ## Right output
    expect_equal(get.common(1,1), 1)
    expect_equal(get.common(c(1,1,1,1),1), 1)
    expect_equal(get.common(c(1,2,3,4),1), 1)
    expect_equal(get.common(c(4,2,5,1),c(2,1)), c(1,2))
    expect_null(get.common(2,1))
    expect_null(get.common(1,2))
})

context("get.union.incl")
test_that("get.union.incl works", {
    ## Not working
    expect_error(get.union.incl(matrix(1), tree))
    expect_error(get.union.incl(1))

    ## Right output
    expect_equal(get.union.incl(1,1), 1)
    expect_equal(get.union.incl(c(1,1,1,1),1), 1)
    expect_equal(get.union.incl(c(1,2,3,4),1), c(1,2,3,4))
    expect_equal(get.union.incl(c(4,2,5,1),c(2,1)), c(1,2,4,5))
    expect_equal(get.union.incl(2,1), c(1,2))
    expect_equal(get.union.incl(1,2), c(1,2))
})

context("get.union.excl")
test_that("get.union.excl works", {
    ## Not working
    expect_error(get.union.excl(matrix(1), tree))
    expect_error(get.union.excl(1))

    ## Right output
    expect_null(get.union.excl(1,1))
    expect_null(get.union.excl(c(1,1,1,1),1))
    expect_equal(get.union.excl(c(1,2,3,4),1), c(2,3,4))
    expect_equal(get.union.excl(c(4,2,5,1),c(2,1)), c(4,5))
    expect_equal(get.union.excl(2,1), c(1,2))
    expect_equal(get.union.excl(1,2), c(1,2))
})

## Trees for testing
tree1 <- read.tree(text = "((((1,2),((3,4),(5,6))),7),8);")
tree2 <- read.tree(text = "((((((((1,2),3),4),5),6),7),8),9);")
tree3 <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")

## Characters for testing
character1 <- "-125-2-1"
character2 <- "01---1010"
character3_1 <- "23--1??--032"
character3_2 <- "1---1111---1"
character3_3 <- "1100----0011"
character3_4 <- "23--1----032"
character3_5 <- "01---1010101"
character3_6 <- "210--100--21"

#########
# Fitch
#########

## Making the matrix
matrix1_1 <- make.states.matrix(tree1, character1, inapplicable = 1)
matrix2_1 <- make.states.matrix(tree2, character2, inapplicable = 1)
matrix3_1_1 <- make.states.matrix(tree3, character3_1, inapplicable = 1)
matrix3_2_1 <- make.states.matrix(tree3, character3_2, inapplicable = 1)
matrix3_3_1 <- make.states.matrix(tree3, character3_3, inapplicable = 1)
matrix3_4_1 <- make.states.matrix(tree3, character3_4, inapplicable = 1)
matrix3_5_1 <- make.states.matrix(tree3, character3_5, inapplicable = 1)
matrix3_6_1 <- make.states.matrix(tree3, character3_6, inapplicable = 1)
matrix1_2 <- make.states.matrix(tree1, character1, inapplicable = 2)
matrix2_2 <- make.states.matrix(tree2, character2, inapplicable = 2)
matrix3_1_2 <- make.states.matrix(tree3, character3_1, inapplicable = 2)
matrix3_2_2 <- make.states.matrix(tree3, character3_2, inapplicable = 2)
matrix3_3_2 <- make.states.matrix(tree3, character3_3, inapplicable = 2)
matrix3_4_2 <- make.states.matrix(tree3, character3_4, inapplicable = 2)
matrix3_5_2 <- make.states.matrix(tree3, character3_5, inapplicable = 2)
matrix3_6_2 <- make.states.matrix(tree3, character3_6, inapplicable = 2)


## Running the pass
pass1_1_1 <- fitch.downpass(matrix1_1, tree1)
pass1_2_1 <- fitch.downpass(matrix2_1, tree2)
pass1_3_1_1 <- fitch.downpass(matrix3_1_1, tree3)
pass1_3_2_1 <- fitch.downpass(matrix3_2_1, tree3)
pass1_3_3_1 <- fitch.downpass(matrix3_3_1, tree3)
pass1_3_4_1 <- fitch.downpass(matrix3_4_1, tree3)
pass1_3_5_1 <- fitch.downpass(matrix3_5_1, tree3)
pass1_3_6_1 <- fitch.downpass(matrix3_6_1, tree3)
pass1_1_2 <- fitch.downpass(matrix1_2, tree1)
pass1_2_2 <- fitch.downpass(matrix2_2, tree2)
pass1_3_1_2 <- fitch.downpass(matrix3_1_2, tree3)
pass1_3_2_2 <- fitch.downpass(matrix3_2_2, tree3)
pass1_3_3_2 <- fitch.downpass(matrix3_3_2, tree3)
pass1_3_4_2 <- fitch.downpass(matrix3_4_2, tree3)
pass1_3_5_2 <- fitch.downpass(matrix3_5_2, tree3)
pass1_3_6_2 <- fitch.downpass(matrix3_6_2, tree3)

context("fitch.downpass")
test_that("fitch.downpass works", {
    results1_fitch1_1 <- list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2))
    results2_fitch1_1 <- list(c(0,1),c(1),c(0,1),c(1),c(0,1),c(0,1),c(0,1),c(0,1))
    results3_1_fitch1_1 <- list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3))
    results3_2_fitch1_1 <- list(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1))
    results3_3_fitch1_1 <- list(c(0),c(0),c(0),c(0),c(0,1),c(1),c(0),c(0),c(0),c(0,1),c(1))
    results3_4_fitch1_1 <- list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3))
    results3_5_fitch1_1 <- list(c(1),c(1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_fitch1_1 <- list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2))
    results1_fitch1_2 <- list(c(1,6),c(6),c(1,2,6),c(1,6),c(2),c(2,5),c(2,6))
    results2_fitch1_2 <- list(c(0,1),c(1),c(0,1,2),c(1,2),c(2),c(2),c(0,1,2),c(0,1))
    results3_1_fitch1_2 <- list(c(4),c(1,4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3))
    results3_2_fitch1_2 <- list(c(1),c(1),c(1,2),c(2),c(2),c(1,2),c(1),c(1,2),c(2),c(2),c(1,2))
    results3_3_fitch1_2 <- list(c(2),c(2),c(0,2),c(0),c(0,1),c(1),c(2),c(0,2),c(0),c(0,1),c(1))
    results3_4_fitch1_2 <- list(c(4),c(4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3))
    results3_5_fitch1_2 <- list(c(1),c(1,2),c(2),c(2),c(0,1,2),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_fitch1_2 <- list(c(0,1,3),c(1,3),c(3),c(0,1,2,3),c(0,1,2),c(1,2),c(0),c(0,3),c(3),c(1,2,3),c(1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_1_1[[2]][node+8], results1_fitch1_1[node])
        expect_equal(pass1_1_2[[2]][node+8], results1_fitch1_2[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_2_1[[2]][node+9], results2_fitch1_1[node])
        expect_equal(pass1_2_2[[2]][node+9], results2_fitch1_2[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_3_1_1[[2]][node+12], results3_1_fitch1_1[node])
        expect_equal(pass1_3_2_1[[2]][node+12], results3_2_fitch1_1[node])
        expect_equal(pass1_3_3_1[[2]][node+12], results3_3_fitch1_1[node])
        expect_equal(pass1_3_4_1[[2]][node+12], results3_4_fitch1_1[node])
        expect_equal(pass1_3_5_1[[2]][node+12], results3_5_fitch1_1[node])
        expect_equal(pass1_3_6_1[[2]][node+12], results3_6_fitch1_1[node])
        expect_equal(pass1_3_1_2[[2]][node+12], results3_1_fitch1_2[node])
        expect_equal(pass1_3_2_2[[2]][node+12], results3_2_fitch1_2[node])
        expect_equal(pass1_3_3_2[[2]][node+12], results3_3_fitch1_2[node])
        expect_equal(pass1_3_4_2[[2]][node+12], results3_4_fitch1_2[node])
        expect_equal(pass1_3_5_2[[2]][node+12], results3_5_fitch1_2[node])
        expect_equal(pass1_3_6_2[[2]][node+12], results3_6_fitch1_2[node])
    }
})


pass2_1_1 <- fitch.uppass(pass1_1_1, tree1)
pass2_2_1 <- fitch.uppass(pass1_2_1, tree2)
pass2_3_1_1 <- fitch.uppass(pass1_3_1_1, tree3)
pass2_3_2_1 <- fitch.uppass(pass1_3_2_1, tree3)
pass2_3_3_1 <- fitch.uppass(pass1_3_3_1, tree3)
pass2_3_4_1 <- fitch.uppass(pass1_3_4_1, tree3)
pass2_3_5_1 <- fitch.uppass(pass1_3_5_1, tree3)
pass2_3_6_1 <- fitch.uppass(pass1_3_6_1, tree3)
pass2_1_2 <- fitch.uppass(pass1_1_2, tree1)
pass2_2_2 <- fitch.uppass(pass1_2_2, tree2)
pass2_3_1_2 <- fitch.uppass(pass1_3_1_2, tree3)
pass2_3_2_2 <- fitch.uppass(pass1_3_2_2, tree3)
pass2_3_3_2 <- fitch.uppass(pass1_3_3_2, tree3)
pass2_3_4_2 <- fitch.uppass(pass1_3_4_2, tree3)
pass2_3_5_2 <- fitch.uppass(pass1_3_5_2, tree3)
pass2_3_6_2 <- fitch.uppass(pass1_3_6_2, tree3)


context("fitch.uppass")
test_that("fitch.uppass works", {
    results1_fitch2_1 <- list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2))
    results2_fitch2_1 <- list(c(0,1),c(1),c(0,1),c(1),c(0,1),c(0,1),c(0,1),c(0,1))
    results3_1_fitch2_1 <- list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3))
    results3_2_fitch2_1 <- list(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1))
    results3_3_fitch2_1 <- list(c(0),c(0),c(0),c(0),c(0,1),c(1),c(0),c(0),c(0),c(0,1),c(1))
    results3_4_fitch2_1 <- list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3))
    results3_5_fitch2_1 <- list(c(1),c(1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_fitch2_1 <- list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2))
    results1_fitch2_2 <- list(c(1,6),c(6),c(1,2,6),c(1,6),c(2),c(2,5),c(2,6))
    results2_fitch2_2 <- list(c(0,1),c(1),c(0,1,2),c(1,2),c(2),c(2),c(0,1,2),c(0,1))
    results3_1_fitch2_2 <- list(c(4),c(1,4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3))
    results3_2_fitch2_2 <- list(c(1),c(1),c(1,2),c(2),c(2),c(1,2),c(1),c(1,2),c(2),c(2),c(1,2))
    results3_3_fitch2_2 <- list(c(2),c(2),c(0,2),c(0),c(0,1),c(1),c(2),c(0,2),c(0),c(0,1),c(1))
    results3_4_fitch2_2 <- list(c(4),c(4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3))
    results3_5_fitch2_2 <- list(c(1),c(1,2),c(2),c(2),c(0,1,2),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_fitch2_2 <- list(c(0,1,3),c(1,3),c(3),c(0,1,2,3),c(0,1,2),c(1,2),c(0),c(0,3),c(3),c(1,2,3),c(1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_1_1[[2]][node+8], results1_fitch2_1[node])
        expect_equal(pass2_1_2[[2]][node+8], results1_fitch2_2[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_2_1[[2]][node+9], results2_fitch2_1[node])
        expect_equal(pass2_2_2[[2]][node+9], results2_fitch2_2[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_3_1_1[[2]][node+12], results3_1_fitch2_1[node])
        expect_equal(pass2_3_2_1[[2]][node+12], results3_2_fitch2_1[node])
        expect_equal(pass2_3_3_1[[2]][node+12], results3_3_fitch2_1[node])
        expect_equal(pass2_3_4_1[[2]][node+12], results3_4_fitch2_1[node])
        expect_equal(pass2_3_5_1[[2]][node+12], results3_5_fitch2_1[node])
        expect_equal(pass2_3_6_1[[2]][node+12], results3_6_fitch2_1[node])
        expect_equal(pass2_3_1_2[[2]][node+12], results3_1_fitch2_2[node])
        expect_equal(pass2_3_2_2[[2]][node+12], results3_2_fitch2_2[node])
        expect_equal(pass2_3_3_2[[2]][node+12], results3_3_fitch2_2[node])
        expect_equal(pass2_3_4_2[[2]][node+12], results3_4_fitch2_2[node])
        expect_equal(pass2_3_5_2[[2]][node+12], results3_5_fitch2_2[node])
        expect_equal(pass2_3_6_2[[2]][node+12], results3_6_fitch2_2[node])
    }
})

#########
# inapplicable
#########

## Making the matrix
matrix1 <- make.states.matrix(tree1, character1)
matrix2 <- make.states.matrix(tree2, character2)
matrix3_1 <- make.states.matrix(tree3, character3_1)
matrix3_2 <- make.states.matrix(tree3, character3_2)
matrix3_3 <- make.states.matrix(tree3, character3_3)
matrix3_4 <- make.states.matrix(tree3, character3_4)
matrix3_5 <- make.states.matrix(tree3, character3_5)
matrix3_6 <- make.states.matrix(tree3, character3_6)

## Running the pass
pass1_1 <- first.downpass(matrix1, tree1)
pass1_2 <- first.downpass(matrix2, tree2)
pass1_3_1 <- first.downpass(matrix3_1, tree3)
pass1_3_2 <- first.downpass(matrix3_2, tree3)
pass1_3_3 <- first.downpass(matrix3_3, tree3)
pass1_3_4 <- first.downpass(matrix3_4, tree3)
pass1_3_5 <- first.downpass(matrix3_5, tree3)
pass1_3_6 <- first.downpass(matrix3_6, tree3)

context("first.downpass")
test_that("first.downpass works", {
    results1_inapp1 <- list(c(1),c(-1,1,2),c(1,2),c(-1,1),c(2),c(2,5),c(-1,2))
    results2_inapp1 <- list(c(0,1),c(1),c(0,1),c(-1,1),c(-1),c(-1),c(-1,0,1),c(0,1))
    results3_1_inapp1 <- list(c(-1),c(-1,1),c(-1,1),c(-1),c(-1,2,3),c(2,3),c(-1),c(-1),c(-1,0,2,3),c(0,2,3),c(2,3))
    results3_2_inapp1 <- list(c(1),c(1),c(-1,1),c(-1),c(-1),c(-1,1),c(1),c(-1,1),c(-1),c(-1),c(-1, 1))
    results3_3_inapp1 <- list(c(-1),c(-1),c(-1,0),c(0),c(0,1),c(1),c(-1),c(-1,0),c(0),c(0,1),c(1)) ## The 8th node is c(-1,1) in morphy! 9th is c(1) in morphy! 
    results3_4_inapp1 <- list(c(-1),c(-1),c(-1,1),c(-1),c(-1,2,3),c(2,3),c(-1),c(-1),c(-1,0,2,3),c(0,2,3),c(2,3))
    results3_5_inapp1 <- list(c(1),c(-1,1),c(-1),c(-1),c(-1,0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_inapp1 <- list(c(0,1),c(-1,1),c(-1),c(-1,0,1,2),c(0,1,2),c(1,2),c(0),c(-1,0),c(-1),c(-1,1,2),c(1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_1[[2]][node+8], results1_inapp1[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_2[[2]][node+9], results2_inapp1[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass1_3_1[[2]][node+12], results3_1_inapp1[node])
        expect_equal(pass1_3_2[[2]][node+12], results3_2_inapp1[node])
        expect_equal(pass1_3_3[[2]][node+12], results3_3_inapp1[node])
        expect_equal(pass1_3_4[[2]][node+12], results3_4_inapp1[node])
        expect_equal(pass1_3_5[[2]][node+12], results3_5_inapp1[node])
        expect_equal(pass1_3_6[[2]][node+12], results3_6_inapp1[node])
    }
})

## Running the pass
pass2_1 <- first.uppass(pass1_1, tree1)
pass2_2 <- first.uppass(pass1_2, tree2)
pass2_3_1 <- first.uppass(pass1_3_1, tree3)
pass2_3_2 <- first.uppass(pass1_3_2, tree3)
pass2_3_3 <- first.uppass(pass1_3_3, tree3)
pass2_3_4 <- first.uppass(pass1_3_4, tree3)
pass2_3_5 <- first.uppass(pass1_3_5, tree3)
pass2_3_6 <- first.uppass(pass1_3_6, tree3)

context("first.uppass")
test_that("first.uppass works", {
    results1_inapp2 <- list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2))
    results2_inapp2 <- list(c(0,1),c(1),c(0,1),c(1),c(-1),c(-1),c(-1),c(0,1))
    results3_1_inapp2 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_2_inapp2 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp2 <- list(c(-1),c(-1),c(-1),c(0),c(0,1),c(1),c(-1),c(-1),c(0),c(0,1),c(1)) # 9th is c(1) in morphy! 
    results3_4_inapp2 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_5_inapp2 <- list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_inapp2 <- list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0),c(1,2),c(1,2),c(1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_1[[3]][node+8], results1_inapp2[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_2[[3]][node+9], results2_inapp2[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass2_3_1[[3]][node+12], results3_1_inapp2[node])
        expect_equal(pass2_3_2[[3]][node+12], results3_2_inapp2[node])
        expect_equal(pass2_3_3[[3]][node+12], results3_3_inapp2[node])
        expect_equal(pass2_3_4[[3]][node+12], results3_4_inapp2[node])
        expect_equal(pass2_3_5[[3]][node+12], results3_5_inapp2[node])
        expect_equal(pass2_3_6[[3]][node+12], results3_6_inapp2[node])
    }

})

## Running the pass
pass3_1 <- second.downpass(pass2_1, tree1)
pass3_2 <- second.downpass(pass2_2, tree2)
pass3_3_1 <- second.downpass(pass2_3_1, tree3)
pass3_3_2 <- second.downpass(pass2_3_2, tree3)
pass3_3_3 <- second.downpass(pass2_3_3, tree3)
pass3_3_4 <- second.downpass(pass2_3_4, tree3)
pass3_3_5 <- second.downpass(pass2_3_5, tree3)
pass3_3_6 <- second.downpass(pass2_3_6, tree3)

context("second.downpass")
test_that("second.downpass works", {
    results1_inapp3 <- list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2))
    results2_inapp3 <- list(c(0,1),c(1),c(0,1),c(1),c(-1),c(-1),c(-1),c(0,1))
    results3_1_inapp3 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_2_inapp3 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp3 <- list(c(-1),c(-1),c(-1),c(0),c(0,1),c(1),c(-1),c(-1),c(0),c(0,1),c(1)) # 9th is c(1) in morphy! 
    results3_4_inapp3 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_5_inapp3 <- list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1))
    results3_6_inapp3 <- list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass3_1[[4]][node+8], results1_inapp3[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass3_2[[4]][node+9], results2_inapp3[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass3_3_1[[4]][node+12], results3_1_inapp3[node])
        expect_equal(pass3_3_2[[4]][node+12], results3_2_inapp3[node])
        expect_equal(pass3_3_3[[4]][node+12], results3_3_inapp3[node])
        expect_equal(pass3_3_4[[4]][node+12], results3_4_inapp3[node])
        expect_equal(pass3_3_5[[4]][node+12], results3_5_inapp3[node])
        expect_equal(pass3_3_6[[4]][node+12], results3_6_inapp3[node])
    }

})

## Running the pass
pass4_1 <- second.uppass(pass3_1, tree1)
pass4_2 <- second.uppass(pass3_2, tree2)
pass4_3_1 <- second.uppass(pass3_3_1, tree3)
pass4_3_2 <- second.uppass(pass3_3_2, tree3)
pass4_3_3 <- second.uppass(pass3_3_3, tree3)
pass4_3_4 <- second.uppass(pass3_3_4, tree3)
pass4_3_5 <- second.uppass(pass3_3_5, tree3)
pass4_3_6 <- second.uppass(pass3_3_6, tree3)

context("second.uppass")
test_that("second.uppass works", {
    results1_inapp4 <- list(c(1),c(1),c(1),c(1),c(2),c(2),c(2))
    results2_inapp4 <- list(c(0,1),c(0,1),c(0,1),c(0,1),c(-1),c(-1),c(-1),c(0,1))
    results3_1_inapp4 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(0,2,3))
    results3_2_inapp4 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp4 <- list(c(-1),c(-1),c(-1),c(0),c(0),c(1),c(-1),c(-1),c(0),c(0),c(1))  # 9th and 10th is c(1) in morphy! 
    results3_4_inapp4 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(0,2,3))
    results3_5_inapp4 <- list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(1),c(1),c(1),c(1),c(1))
    results3_6_inapp4 <- list(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1,2),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1,2))

    ## Testing tree 2
    for(node in 1:7) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass4_1[[5]][node+8], results1_inapp4[node])
    }

    ## Testing tree 2
    for(node in 1:8) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass4_2[[5]][node+9], results2_inapp4[node])
    }
    
    ## Testing tree 3
    for(node in 1:11) {
        # cat(paste(node, " ", sep =""))
        expect_equal(pass4_3_1[[5]][node+12], results3_1_inapp4[node])
        expect_equal(pass4_3_2[[5]][node+12], results3_2_inapp4[node])
        expect_equal(pass4_3_3[[5]][node+12], results3_3_inapp4[node])
        expect_equal(pass4_3_4[[5]][node+12], results3_4_inapp4[node])
        expect_equal(pass4_3_5[[5]][node+12], results3_5_inapp4[node])
        expect_equal(pass4_3_6[[5]][node+12], results3_6_inapp4[node])
    }
})


context("correct step counting")
test_that("right counting", {
    ## Tree
    tree <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    characters <- c("23--1??--032" # 1
                    ,"1---1111---1" # 2
                    ,"1100----1100" # 3
                    ,"11-------100" # 4
                    ,"----1111---1" # 5
                    ,"01----010101" # 6 
                    ,"01---1010101" # 7
                    ,"1??--??--100" # 8
                    ,"21--3??--032" # 9 #TG: directional counting problem! If uppass is right/left, counts 1, else counts 2 (correct answer)
                    ,"11--1??--111" # 10
                    ,"11--1000001-" # 11
                    ,"01------0101" # 12
                    ,"110--?---100" # 13
                    ,"11--1??--111" # 14
                    ,"210--100--21" # 15
                    ,"????----1???" # 16
                    ,"23--1----032" # 17
                    ,"1----1----1-" # 18
                    ,"-1-1-1--1-1-" # 19
                    ,"23--1??--032" # 20
                    ,"--------0101" # 21
                    ,"10101-----01" # 22
                    ,"011--?--0011" # 23
                    ,"110--??--100" # 24
                    ,"11--1000001-" # 25
                    ,"21--1----012" # 26
                    ,"11----111111" # 27
                    ,"10101-----01" # 28
                    ,"210210------" # 29
                    ,"----1111----" # 30
                    ,"230--??1--32" # 31
                    ,"023--??1--32" # 32
                    ,"023-???1--32" # 33
                    ,"23--1?1--023" # 34
                    ,"----1010----" # 35
                    ,"------11---1" # 36
                    ,"10----11---1" # 37
                    ,"1---------01") # 38
    ## Results
    expected_results <- c(2, 2, 2, 1, 1, 4, 4, 1, 2, 2, 1, 3, 2, 2, 3, 0, 2, 2, 4, 2, 1, 3, 2, 2, 1, 3, 1, 3, 2, 0, 2, 2, 1, 2, 1, 1, 2, 1)

    ## Run the tests
    for(test in 1:length(characters)) { #9 and 33 are still bugged!
        suppressWarnings(matrix <- inapplicable.algorithm(tree, characters[test], passes = 4, method = "Inapplicable", inapplicable = NULL))
        # if(matrix$length != expected_results[test]) {
        #     print(paste("test", test, "failed"))
        #     print(paste("Is", matrix$length, "instead of", expected_results[test]))
        # }
        expect_equal(matrix$length, expected_results[test])
    }
})

context("inapplicable.algorithm")
test_that("inapplicable.algorithm works", {
    ## Test the three cases
})

context("plot.convert.state")
test_that("plot.convert.state works", {

})

