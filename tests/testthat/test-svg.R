context("svg functions")
library('ape')

test_that('SVG can be plotted', {
    trees <- structure(list(
      tree1 = read.tree(text='((a, b), ((c, (d, (e, f))), ((g, h), (i, j))));'),
      tree2 = read.tree(text='((a, b), ((c, (d, (e, f))), ((g, i), (h, j))));'),
      tree3 = read.tree(text='(a, (b, ((c, (d, (e, h))), ((g, f), (i, j)))));'),
      tree1again = read.tree(text='((a, b), ((c, (d, (e, f))), ((g, h), (i, j))));')),
      class='multiPhylo')

    treesGeneratedBy <- c('text', 'text', 'text', 'duplication')

    canvas <- SVGCanvas(trees=trees,
                     outgroupTips=c('a', 'b'),
                     analysisNames=treesGeneratedBy,
                     width=300, height=300)

    PlotExample <- function ()
    PlotCharacterMapping(char = '0011----11',
                      stateLabels = c('Absent', 'Present'),
                      singleTree = trees[[1]],
                      legendText = 'This is printed on PNGs',
                      canvas = canvas,
                      svgFilename = 'tree_number_%s.svg')
  expect_doppelganger('SVG example', PlotExample)
})
