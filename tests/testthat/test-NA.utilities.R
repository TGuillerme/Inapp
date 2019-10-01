require(ape)

## convert.char
context("convert.char")
test_that("convert.char works", {
    character <- "01---?010101"
    list_out <- list(0,1,-1,-1,-1,c(-1,0,1),0,1,0,1,0,1)

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
    test <- convert.char(character)
    for(elem in 1:length(list_out)) {
        expect_true(all(test[[elem]] == list_out[[elem]]))
    }

    expect_true(unlist(test[1]) == 0)
    expect_true(unlist(test[2]) == 1)
    expect_true(unlist(test[3]) == -1)
    expect_true(all(unlist(test[6]) == c(-1,0,1)))

    ## Complex conversion
    character <- "{01}{-0}?-01"
    list_out <- list(c(0,1), c(-1,0), c(-1,0,1), -1, 0, 1)
    test <- convert.char(character)
    for(elem in 1:length(list_out)) {
        expect_true(all(test[[elem]] == list_out[[elem]]))
    }
})

context("make.states.matrix")
test_that("make.states.matrix works", {
    tree <- read.tree(text='(t3, (t4, (t1, t2)));')
    character <- "01-?"

    ## Not working (error)
    expect_error(make.states.matrix("tree", character, inapplicable = NULL))
    expect_error(make.states.matrix(tree, 1, inapplicable = NULL))

    ## Right output style
    matrix <- make.states.matrix(tree, character)
    expect_is(matrix, "states.matrix")
    expect_equal(unlist(lapply(matrix, class)),
                 c(Char = "list",
                   Dp1 = 'list',
                   Up1 = 'list',
                   Dp2 = 'list',
                   Up2 = 'list',
                   tracker = 'list',
                   regions = "integer",
                   changes = 'integer',
                   score = 'integer',
                   tree = "phylo",
                   n_tip = 'integer',
                   n_node = 'integer'))
    expect_equal(unique(unlist(lapply(matrix, length))), c(7,4,0,3,1))
    expect_equal(length(matrix), 12)
    expect_equal(ape::Ntip(matrix$tree), matrix$n_tip)
    expect_equal(ape::Nnode(matrix$tree), matrix$n_node)

    ## Right output values
    expect_equal(c(0,1,0,1,0,1),
                 unlist(make.states.matrix(tree, character, inapplicable = 1)[[1]]))
    expect_equal(list(2, 0:2, 0, 1, NULL, NULL, NULL),
                 lapply(make.states.matrix(tree, character, inapplicable = 2,
                                           match.tip.char = TRUE)[[1]], sort))


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
    answers <- list(c(5), c(7), c(7), c(6), c(1, 6), c(7, 4, 5), c(2, 3, 6))
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



context("print.states.matrix")
test_that("print.states.matrix works", {

    tree <- ape::read.tree(text = "((a,b),(c,d));")
    character <- "01?-"
    test <- make.states.matrix(tree, character)
    out <- capture.output(test)

    expect_equal(out, c(" ---- Tree ---- " , "((a,b),(c,d)); "  , " ---- States matrix---- " , "Number of tips = 4 "  , "Character states = -, 0, 1 "  , "No reconstructions calculated. See:"  , " ?apply.reconstruction" , "to reconstruct ancestral states and score the tree."))
})
