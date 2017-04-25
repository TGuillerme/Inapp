library(ape)

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