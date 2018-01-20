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
pass1_1 <- first.downpass(matrix1)
pass1_2 <- first.downpass(matrix2)
pass1_3_1 <- first.downpass(matrix3_1)
pass1_3_2 <- first.downpass(matrix3_2)
pass1_3_3 <- first.downpass(matrix3_3)
pass1_3_4 <- first.downpass(matrix3_4)
pass1_3_5 <- first.downpass(matrix3_5)
pass1_3_6 <- first.downpass(matrix3_6)

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
pass2_1 <- first.uppass(pass1_1)
pass2_2 <- first.uppass(pass1_2)
pass2_3_1 <- first.uppass(pass1_3_1)
pass2_3_2 <- first.uppass(pass1_3_2)
pass2_3_3 <- first.uppass(pass1_3_3)
pass2_3_4 <- first.uppass(pass1_3_4)
pass2_3_5 <- first.uppass(pass1_3_5)
pass2_3_6 <- first.uppass(pass1_3_6)

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
pass3_1 <- second.downpass(pass2_1)
pass3_2 <- second.downpass(pass2_2)
pass3_3_1 <- second.downpass(pass2_3_1)
pass3_3_2 <- second.downpass(pass2_3_2)
pass3_3_3 <- second.downpass(pass2_3_3)
pass3_3_4 <- second.downpass(pass2_3_4)
pass3_3_5 <- second.downpass(pass2_3_5)
pass3_3_6 <- second.downpass(pass2_3_6)

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
pass4_1 <- second.uppass(pass3_1)
pass4_2 <- second.uppass(pass3_2)
pass4_3_1 <- second.uppass(pass3_3_1)
pass4_3_2 <- second.uppass(pass3_3_2)
pass4_3_3 <- second.uppass(pass3_3_3)
pass4_3_4 <- second.uppass(pass3_3_4)
pass4_3_5 <- second.uppass(pass3_3_5)
pass4_3_6 <- second.uppass(pass3_3_6)

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

context("correct tree scoring")
test_that("trees are scored correctly", {
  characters <- c("23--1??--032=5", # 0,
                  "1---1111---1=2", # 1,
                  "1100----1100=3", # 2,
                  "11-------100=2", # 3,
                  "----1111---1=1", # 4,
                  "01----010101=5", # 5,
                  "01---1010101=5", # 6,
                  "1??--??--100=2", # 7,
                  "21--3??--032=5", # 8,
                  "11--1??--111=2", # 9,
                  "11--1000001-=2", # 10,
                  "01------0101=4", # 11
                  "110--?---100=3", # 12
                  "11--1??--111=2", # 13
                  "210--100--21=5", # 14
                  "????----1???=0", # 15
                  "23--1----032=5", # 16
                  "1----1----1-=2", # 17
                  "-1-1-1--1-1-=4", # 18
                  "23--1??--032=5", # 19
                  "--------0101=2", # 20
                  "10101-----01=4", # 21
                  "011--?--0011=3", # 22
                  "110--??--100=3", # 23
                  "11--1000001-=2", # 24
                  "21--1----012=5", # 25
                  "11----111111=1", # 26
                  "10101-----01=4", # 27
                  "210210------=4", # 28
                  "----1111----=0", # 29
                  "230--??1--32=5", # 30
                  "023--??1--32=5", # 31
                  "023-???1--32=4", # 32
                  "23--1?1--023=5", # 33
                  "----1010----=2", # 34
                  "------11---1=1", # 35
                  "10----11---1=3", # 36
                  "320--??3--21=5", # 37
                  "000011110000=2"  # 38
  )
  
  for (character_score in characters) {
    character <- unlist(strsplit(character_score, "=", fixed=TRUE))
    expected_score <- as.integer(character[2])
    matrix <- make.states.matrix(tree3, character[1])
    matrix <- second.uppass(second.downpass(first.uppass(first.downpass(matrix))))
    expect_equal(expected_score, matrix$score)
    if (length(matrix$uppassRegions)) {
      expect_equal(length(matrix$uppassRegions), length(matrix$downpas))
      if (!identical(sort(matrix$uppassRegions), sort(matrix$downpassRegions))) {
        expect_equal("Problematic character:", character[1])
        expect_identical(sort(matrix$uppassRegions), sort(matrix$downpassRegions))
      }
    }
  }
})