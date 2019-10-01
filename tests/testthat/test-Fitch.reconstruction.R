library(ape)
context("preparing Fitch matrices")
test_that('Fitch passes work', {
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
    expect_is(convert.char(character3_1), 'list')
    expect_is(convert.char(character3_2), 'list')
    expect_is(convert.char(character3_3), 'list')
    expect_is(convert.char(character3_4), 'list')
    expect_is(convert.char(character3_5), 'list')
    expect_is(convert.char(character3_6), 'list')
  })


  TestFitchPasses <- function (tree, character, expect_down, expect_up, ...) {
    mat <- make.states.matrix(tree, character, ...)
    tips <- seq_along(tree$tip.label)
    down <- fitch.downpass(mat)
    expect_equal(down$Dp1[-tips], expect_down)

    up <- fitch.uppass(down)
    expect_equal(up$Dp1[-tips], expect_up)
  }

  TestFitchPasses(tree1, character1, inapplicable = 1, match.tip.char = TRUE,
                  expect_down = list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2)),
                  expect_up = list(c(1),c(1,2),c(1,2),c(1),c(2),c(2,5),c(2))
  )

  TestFitchPasses(tree2, character2, inapplicable = 1, match.tip.char = TRUE,
    expect_down = list(c(0,1),c(1),c(0,1),c(1),c(0,1),c(0,1),c(0,1),c(0,1)),
    expect_up  = list(c(0,1),c(1),c(0,1),c(1),c(0,1),c(0,1),c(0,1),c(0,1)))

  TestFitchPasses(tree3, character3_1, inapplicable = 1,
    expect_down = list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3)),
    expect_up = list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3)))
  TestFitchPasses(tree3, character3_2, inapplicable = 1,
    expect_down = list(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1)),
    expect_up = list(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1)))
  TestFitchPasses(tree3, character3_3, inapplicable = 1,
    expect_down = list(c(0),c(0),c(0),c(0),c(0,1),c(1),c(0),c(0),c(0),c(0,1),c(1)),
    expect_up = list(c(0),c(0),c(0),c(0),c(0,1),c(1),c(0),c(0),c(0),c(0,1),c(1)))
  TestFitchPasses(tree3, character3_4, inapplicable = 1,
    expect_down = list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3)),
    expect_up = list(c(2,3),c(1,2,3),c(1,2,3),c(2,3),c(2,3),c(2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(0,2,3),c(2,3)))
  TestFitchPasses(tree3, character3_5, inapplicable = 1,
    expect_down = list(c(1),c(1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)),
    expect_up = list(c(1),c(1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)))
  TestFitchPasses(tree3, character3_6, inapplicable = 1,
    expect_down = list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2)),
    expect_up = list(c(0,1),c(1),c(0,1,2),c(0,1,2),c(0,1,2),c(1,2),c(0),c(0,1,2),c(1,2),c(1,2),c(1,2)))

  TestFitchPasses(tree1, character1, inapplicable = 2, match.tip.char = TRUE,
    expect_down = list(c(1,6),c(6),c(1,2,6),c(1,6),c(2),c(2,5),c(2,6)),
    expect_up = list(c(1,6),c(6),c(1,2,6),c(1,6),c(2),c(2,5),c(2,6)))
  TestFitchPasses(tree2, character2, inapplicable = 2, match.tip.char = TRUE,
    expect_down = list(c(0,1),c(1),c(0,1,2),c(1,2),c(2),c(2),c(0,1,2),c(0,1)),
    expect_up = list(c(0,1),c(1),c(0,1,2),c(1,2),c(2),c(2),c(0,1,2),c(0,1)))

  TestFitchPasses(tree3, character3_1, inapplicable = 2,
    expect_down = list(c(4),c(1,4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3)),
      expect_up = list(c(4),c(1,4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3)))
  TestFitchPasses(tree3, character3_2, inapplicable = 2,
    expect_down = list(c(1),c(1),c(1,2),c(2),c(2),c(1,2),c(1),c(1,2),c(2),c(2),c(1,2)),
      expect_up = list(c(1),c(1),c(1,2),c(2),c(2),c(1,2),c(1),c(1,2),c(2),c(2),c(1,2)))
  TestFitchPasses(tree3, character3_3, inapplicable = 2,
    expect_down = list(c(2),c(2),c(0,2),c(0),c(0,1),c(1),c(2),c(0,2),c(0),c(0,1),c(1)),
    expect_up = list(c(2),c(2),c(0,2),c(0),c(0,1),c(1),c(2),c(0,2),c(0),c(0,1),c(1)))
  TestFitchPasses(tree3, character3_4, inapplicable = 2,
    expect_down = list(c(4),c(4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3)),
    expect_up = list(c(4),c(4),c(1,4),c(4),c(2,3,4),c(2,3),c(4),c(4),c(0,2,3,4),c(0,2,3),c(2,3)))
  TestFitchPasses(tree3, character3_5, inapplicable = 2,
    expect_down = list(c(1),c(1,2),c(2),c(2),c(0,1,2),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)),
    expect_up = list(c(1),c(1,2),c(2),c(2),c(0,1,2),c(0,1),c(0,1),c(1),c(0,1),c(1),c(0,1)))
  TestFitchPasses(tree3, character3_6, inapplicable = 2,
    expect_down = list(c(0,1,3),c(1,3),c(3),c(0,1,2,3),c(0,1,2),c(1,2),c(0),c(0,3),c(3),c(1,2,3),c(1,2)),
    expect_up = list(c(0,1,3),c(1,3),c(3),c(0,1,2,3),c(0,1,2),c(1,2),c(0),c(0,3),c(3),c(1,2,3),c(1,2)))

})
