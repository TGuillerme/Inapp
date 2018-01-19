context("Fitch reconstruction - setup")

library(ape)
## Trees for testing
tree1 <- read.tree(text = "((((1,2),((3,4),(5,6))),7),8);")
tree2 <- read.tree(text = "((((((((1,2),3),4),5),6),7),8),9);")
tree3 <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")

## Characters for testing
character1   <- "-125-2-1"
character2   <- "01---1010"
character3_1 <- "23--1??--032"
character3_2 <- "1---1111---1"
character3_3 <- "1100----0011"
character3_4 <- "23--1----032"
character3_5 <- "01---1010101"
character3_6 <- "210--100--21"

test_that("Characters can be parsed", {
  expect_is(convert.char(character1  ), 'list')
  expect_is(convert.char(character2  ), 'list')
  expect_is(convert.char(character3_1), 'list') # Triggering warning
  expect_is(convert.char(character3_2), 'list')
  expect_is(convert.char(character3_3), 'list')
  expect_is(convert.char(character3_4), 'list')
  expect_is(convert.char(character3_5), 'list')
  expect_is(convert.char(character3_6), 'list')
})

#########
# Fitch
#########

## Making the matrix
matrix1_1 <- make.states.matrix(tree1, character1, inapplicable = 1, match.tip.char = TRUE)
matrix2_1 <- make.states.matrix(tree2, character2, inapplicable = 1, match.tip.char = TRUE)
matrix3_1_1 <- make.states.matrix(tree3, character3_1, inapplicable = 1)
matrix3_2_1 <- make.states.matrix(tree3, character3_2, inapplicable = 1)
matrix3_3_1 <- make.states.matrix(tree3, character3_3, inapplicable = 1)
matrix3_4_1 <- make.states.matrix(tree3, character3_4, inapplicable = 1)
matrix3_5_1 <- make.states.matrix(tree3, character3_5, inapplicable = 1)
matrix3_6_1 <- make.states.matrix(tree3, character3_6, inapplicable = 1)
matrix1_2 <- make.states.matrix(tree1, character1, inapplicable = 2, match.tip.char = TRUE)
matrix2_2 <- make.states.matrix(tree2, character2, inapplicable = 2, match.tip.char = TRUE)
matrix3_1_2 <- make.states.matrix(tree3, character3_1, inapplicable = 2)
matrix3_2_2 <- make.states.matrix(tree3, character3_2, inapplicable = 2)
matrix3_3_2 <- make.states.matrix(tree3, character3_3, inapplicable = 2)
matrix3_4_2 <- make.states.matrix(tree3, character3_4, inapplicable = 2)
matrix3_5_2 <- make.states.matrix(tree3, character3_5, inapplicable = 2)
matrix3_6_2 <- make.states.matrix(tree3, character3_6, inapplicable = 2)


## Running the pass
pass1_1_1 <- fitch.downpass(matrix1_1)
pass1_2_1 <- fitch.downpass(matrix2_1)
pass1_3_1_1 <- fitch.downpass(matrix3_1_1)
pass1_3_2_1 <- fitch.downpass(matrix3_2_1)
pass1_3_3_1 <- fitch.downpass(matrix3_3_1)
pass1_3_4_1 <- fitch.downpass(matrix3_4_1)
pass1_3_5_1 <- fitch.downpass(matrix3_5_1)
pass1_3_6_1 <- fitch.downpass(matrix3_6_1)
pass1_1_2 <- fitch.downpass(matrix1_2)
pass1_2_2 <- fitch.downpass(matrix2_2)
pass1_3_1_2 <- fitch.downpass(matrix3_1_2)
pass1_3_2_2 <- fitch.downpass(matrix3_2_2)
pass1_3_3_2 <- fitch.downpass(matrix3_3_2)
pass1_3_4_2 <- fitch.downpass(matrix3_4_2)
pass1_3_5_2 <- fitch.downpass(matrix3_5_2)
pass1_3_6_2 <- fitch.downpass(matrix3_6_2)

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


pass2_1_1 <- fitch.uppass(pass1_1_1)
pass2_2_1 <- fitch.uppass(pass1_2_1)
pass2_3_1_1 <- fitch.uppass(pass1_3_1_1)
pass2_3_2_1 <- fitch.uppass(pass1_3_2_1)
pass2_3_3_1 <- fitch.uppass(pass1_3_3_1)
pass2_3_4_1 <- fitch.uppass(pass1_3_4_1)
pass2_3_5_1 <- fitch.uppass(pass1_3_5_1)
pass2_3_6_1 <- fitch.uppass(pass1_3_6_1)
pass2_1_2 <- fitch.uppass(pass1_1_2)
pass2_2_2 <- fitch.uppass(pass1_2_2)
pass2_3_1_2 <- fitch.uppass(pass1_3_1_2)
pass2_3_2_2 <- fitch.uppass(pass1_3_2_2)
pass2_3_3_2 <- fitch.uppass(pass1_3_3_2)
pass2_3_4_2 <- fitch.uppass(pass1_3_4_2)
pass2_3_5_2 <- fitch.uppass(pass1_3_5_2)
pass2_3_6_2 <- fitch.uppass(pass1_3_6_2)


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

