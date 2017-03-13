library(testthat)

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
    expect_true(unlist(convert.char("01-?")[1]) == 0)
    expect_true(unlist(convert.char("01-?")[2]) == 1)
    expect_true(unlist(convert.char("01-?")[3]) == -1)
    expect_true(all(unlist(convert.char("01-?")[4]) == c(-1,0,1)))
})

context("make.states.matrix")
test_that("make.states.matrix works", {
    tree <- rtree(4)
    character <- "01-?"

    ## Not working (error)
    expect_error(make.states.matrix("tree", character, inapplicable = NULL))
    expect_error(make.states.matrix(tree, 1, inapplicable = NULL))

    ## Right output style
    matrix <- make.states.matrix(tree, character)
    expect_is(matrix, "list")
    expect_equal(unique(unlist(lapply(matrix, class))), "list")
    expect_equal(unique(unlist(lapply(matrix, length))), 7)
    expect_equal(length(matrix), 5)
    expect_equal(names(matrix), c("Char", "Dp1", "Up1", "Dp2", "Up2"))

    ## Right output values
    expect_equal(unlist(make.states.matrix(tree, character)[[1]]), c(0,1,-1,-1,0,1))
    expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 1)[[1]]), c(0,1,0,1,0,1))
    expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 2)[[1]]), c(0,1,2,0,1,2))
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
    expect_null(get.union.excl(2,1))
    expect_null(get.union.excl(1,2))
})

## Trees for testing
tree1 <- read.tree(text = "((((1,2),((3,4),(5,6))),7),8);")
tree2 <- read.tree(text = "((((((((1,2),3),4),5),6),7),8),9);")
tree3 <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")

character1 <- "-125-2-1"
character2 <- "01---1010"
character3_1 <- "23--1??--032"
character3_2 <- "1---1111---1"
character3_3 <- "1100----0011"
character3_4 <- "23--1----032"
character3_5 <- "01---1010101"

context("fitch.downpass")
test_that("fitch.downpass works", {
    results1_fitch1 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_2_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_3_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_4_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_fitch1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
})

context("fitch.uppass")
test_that("fitch.uppass works", {
    results1_fitch2 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_2_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_3_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_4_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_fitch2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())

})

context("first.downpass")
test_that("first.downpass works", {
    results1_inapp1 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_inapp1 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_inapp1 <- list(c(-1),c(1,-1),c(1,-1),c(-1),c(-1,2,3),c(2,3),c(-1),c(-1),c(-1,0,2,3),c(0,2,3),c(2,3))
    results3_2_inapp1 <- list(c(1),c(1),c(-1,1),c(-1),c(-1),c(-1,1),c(1),c(-1,1),c(-1),c(-1),c(-1, 1))
    results3_3_inapp1 <- list(c(-1),c(-1),c(-1,0),c(0),c(0,1),c(1),c(-1),c(-1,1),c(1),c(0,1),c(0))
    results3_4_inapp1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_inapp1 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
})

context("first.uppass")
test_that("first.uppass works", {
    results1_inapp2 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_inapp2 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_inapp2 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_2_inapp2 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp2 <- list(c(-1),c(-1),c(-1),c(0),c(0,1),c(1),c(-1),c(-1),c(1),c(0,1),c(1))
    results3_4_inapp2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_inapp2 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
})

context("second.downpass")
test_that("second.downpass works", {
    results1_inapp3 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_inapp3 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_inapp3 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3))
    results3_2_inapp3 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp3 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_4_inapp3 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_inapp3 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
})

context("second.uppass")
test_that("second.uppass works", {
    results1_inapp4 <- list(c(),c(),c(),c(),c(),c(),c())
    results2_inapp4 <- list(c(),c(),c(),c(),c(),c(),c(),c())
    results3_1_inapp4 <- list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3,0))
    results3_2_inapp4 <- list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
    results3_3_inapp4 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_4_inapp4 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
    results3_5_inapp4 <- list(c(),c(),c(),c(),c(),c(),c(),c(),c(),c(),c())
})

context("inapplicable.algorithm")
test_that("inapplicable.algorithm works", {
    ## Test the three cases
})

context("plot.convert.statem")
test_that("plot.convert.statem works", {

})

