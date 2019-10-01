library(ape)
context('Inapplicable reconstruction')

test_that('Inapplicable passes work', {
  ## Trees for testing
  tree1 <- read.tree(text = "((((1,2),((3,4),(5,6))),7),8);")
  tree2 <- read.tree(text = "((((((((1,2),3),4),5),6),7),8),9);")
  tree3 <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")

  ## Characters for testing - duplicated from test-Fitch.reconstruction.R
  character1   <- "-125-2-1"
  character2   <- "01---1010"
  character3_1 <- "23--1??--032"
  character3_2 <- "1---1111---1"
  character3_3 <- "1100----0011"
  character3_4 <- "23--1----032"
  character3_5 <- "01---1010101"
  character3_6 <- "210--100--21"


  TestInappPasses <- function (tree, character,
                               expect_dp1, expect_up1,
                               expect_dp2, expect_up2) {
    mat <- make.states.matrix(tree, character)
    tips <- seq_along(tree$tip.label)
    dp1 <- first.downpass(mat)
    expect_equal(dp1$Dp1[-tips], expect_dp1)

    up1 <- first.uppass(dp1)
    expect_equal(up1$Up1[-tips], expect_up1)

    dp2 <- second.downpass(up1)
    expect_equal(dp2$Dp2[-tips], expect_dp2)

    up2 <- second.uppass(dp2)
    expect_equal(up2$Up2[-tips], expect_up2)
  }

  TestInappPasses(tree1, character1,
    expect_dp1 = list(c(1),c(-1,1,2),c(1,2),c(-1,1),c(2),c(2,5),c(-1,2)),
    expect_up1 = list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2)),
    expect_dp2 = list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2)),
    expect_up2 = list(c(1),c(1),c(1),c(1),c(2),c(2),c(2))
  )
  TestInappPasses(tree2, character2,
    expect_dp1 = list(c(0,1),c(1),c(0,1),c(-1,1),c(-1),c(-1),c(-1,0,1),c(0,1)),
    expect_up1 = list(c(0,1),c(1),c(0,1),c(1),c(-1),c(-1),c(-1),c(0,1)),
    expect_dp2 = list(c(0,1),c(1),c(0,1),c(1),c(-1),c(-1),c(-1),c(0,1)),
    expect_up2 = list(c(0,1),c(0,1),c(0,1),c(0,1),c(-1),c(-1),c(-1),c(0,1))
  )
  TestInappPasses(tree3, character3_1,
    expect_dp1 = list(c(-1),c(-1,1),c(-1,1),c(-1),c(-1,2,3),c(2,3),c(-1),c(-1),c(-1,0,2,3),c(0,2,3),c(2,3)),
    expect_up1 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3)),
    expect_dp2 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3)),
    expect_up2 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(0,2,3))
  )
  TestInappPasses(tree3, character3_2,
    expect_dp1 = list(c(1),c(1),c(-1,1),c(-1),c(-1),c(-1,1),c(1),c(-1,1),c(-1),c(-1),c(-1, 1)),
    expect_up1 = list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1)),
    expect_dp2 = list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1)),
    expect_up2 = list(c(1),c(1),c(1),c(-1),c(-1),c(-1),c(1),c(1),c(-1),c(-1),c(-1))
  )
  TestInappPasses(tree3, character3_3,
    expect_dp1 = list(c(-1),c(-1),c(-1,0),c(0),c(0,1),c(1),c(-1),c(-1,0),c(0),c(0,1),c(1)), ## The 8th node is c(-1,1) in morphy! 9th is c(1) in morphy!
    expect_up1 = list(c(-1),c(-1),c(-1),c(0),c(0,1),c(1),c(-1),c(-1),c(0),c(0,1),c(1)), # 9th is c(1) in morphy!
    expect_dp2 = list(c(-1),c(-1),c(-1),c(0),c(0,1),c(1),c(-1),c(-1),c(0),c(0,1),c(1)), # 9th is c(1) in morphy!
    expect_up2 = list(c(-1),c(-1),c(-1),c(0),c(0),c(1),c(-1),c(-1),c(0),c(0),c(1))  # 9th and 10th is c(1) in morphy!
  )
  TestInappPasses(tree3, character3_4,
    expect_dp1 = list(c(-1),c(-1),c(-1,1),c(-1),c(-1,2,3),c(2,3),c(-1),c(-1),c(-1,0,2,3),c(0,2,3),c(2,3)),
    expect_up1 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3)),
    expect_dp2 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(2,3)),
    expect_up2 = list(c(-1),c(-1),c(-1),c(-1),c(-1),c(2,3),c(-1),c(-1),c(-1),c(0,2,3),c(0,2,3))
  )
  TestInappPasses(tree3, character3_5,
    expect_dp1 = list(c(1),c(-1,1),c(-1),c(-1),c(-1,0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)),
    expect_up1 = list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)),
    expect_dp2 = list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)),
    expect_up2 = list(c(1),c(1),c(-1),c(-1),c(-1),c(0,1),c(1),c(1),c(1),c(1),c(1))
  )
  TestInappPasses(tree3, character3_6,
    expect_dp1 = list(c(0,1),c(-1,1),c(-1),c(-1,0,1,2),c(0,1,2),c(1,2),c(0),c(-1,0),c(-1),c(-1,1,2),c(1,2)),
    expect_up1 = list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0),c(1,2),c(1,2),c(1,2)),
    expect_dp2 = list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2)),
    expect_up2 = list(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1,2),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1,2))
  )
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
    matrix <- second.downpass(first.uppass(first.downpass(matrix)))
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
