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
    set.seed(1)
    tree <- rtree(4, br = NULL)
    character <- "01-?"

    ## Not working (error)
    expect_error(make.states.matrix("tree", character, inapplicable = NULL))
    expect_error(make.states.matrix(tree, 1, inapplicable = NULL))

    ## Right output style
    expect_warning(matrix <- make.states.matrix(tree, character))  # Warning is some weird NA management by testthat
    expect_is(matrix, "states.matrix")
    expect_equal(unique(unlist(lapply(matrix, class))), c("list", "numeric", "phylo"))
    expect_equal(unique(unlist(lapply(matrix, length))), c(7,4,1,0,3))
    expect_equal(length(matrix), 9)
    expect_equal(names(matrix), c("Char", "Dp1", "Up1", "Dp2", "Up2", "tracker", "regions", "changes", "tree"))

    ## Right output values
    expect_warning(expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 1)[[1]]), c(0,1,0,1,0,1)))  # Warning is some weird NA management by testthat
    expect_warning(expect_equal(unlist(make.states.matrix(tree, character, inapplicable = 2, match.tip.char = TRUE)[[1]]), c(2,0,1,2,0,1)))  # Warning is some weird NA management by testthat


    ## Matching the character to the tips
    tree1 <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    tree2 <- read.tree(text = "((((((12,8),5),7),1),11),(2,(6,(3,(9,(4,10))))));")
    character <- "123456789012"
    expected1 <- c(1,2,3,4,5,6,7,8,9,0,1,2)
    expected2 <- c(2,8,5,7,1,1,2,6,3,9,4,0)

    ## Doesn't matter if tips are ordered
    expect_true(all(unlist(make.states.matrix(tree1, character, match.tip.char = TRUE)[[1]]) == unlist(make.states.matrix(tree1, character, match.tip.char = FALSE)[[1]])))
    expect_true(all(unlist(make.states.matrix(tree1, character, match.tip.char = TRUE)[[1]]) == expected1))
    expect_true(all(unlist(make.states.matrix(tree1, character, match.tip.char = FALSE)[[1]]) == expected1))

    ## Does matters otherwise
    expect_false(all(unlist(make.states.matrix(tree2, character, match.tip.char = TRUE)[[1]]) == unlist(make.states.matrix(tree2, character, match.tip.char = FALSE)[[1]])))
    ## The first one should match 1234... and the second one the tip order
    expect_true(all(unlist(make.states.matrix(tree2, character, match.tip.char = FALSE)[[1]]) == expected1))
    expect_true(all(unlist(make.states.matrix(tree2, character, match.tip.char = TRUE)[[1]]) == expected2))

    ## Matching the character to the tips (2)
    tree1 <- read.tree(text = "((1,2),3);")
    tree2 <- read.tree(text = "((3,1),2);")
    character <- "123"
    expect_true(all(unlist(make.states.matrix(tree1, character, match.tip.char = FALSE)$Char) == c(1,2,3)))
    expect_true(all(unlist(make.states.matrix(tree1, character, match.tip.char = TRUE)$Char) == c(1,2,3)))
    expect_true(all(unlist(make.states.matrix(tree2, character, match.tip.char = FALSE)$Char) == c(1,2,3)))
    expect_true(all(unlist(make.states.matrix(tree2, character, match.tip.char = TRUE)$Char) == c(3,1,2)))
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