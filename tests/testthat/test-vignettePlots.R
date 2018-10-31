context("vignettePlots and svg")

## Test
test_that("vignettePlots works", {

    ## Random vignette tests
    hasTail <- ape::read.tree(text="((a, (b, (c, d))), (e, ((f, (g, X)), (h, i))));")
    tail <- "000001111?"
    ap <- c("Absent", "Present")
    expect_null(vignettePlot(hasTail, tail, legend.pos='topleft', na=FALSE, main="Tail", state.labels = ap))
    rb <- c('Red', 'Blue')
    orb <- c('', rb)
    expect_null(vignettePlot(ape::read.tree(text="((a, (i1, i2)), (b, (c, (d, (e, (f, (g, h)))))));"),
             '---11122??', legend.pos='topleft', state.labels = orb, passes=4))

    ## Overiding
    expect_null(vignettePlot(hasTail, tail, legend.pos='topleft', na=FALSE, main="Tail", state.labels = ap,
                            state.override = replicate(19, list(1)),
                            score.override = 2,
                            changes.override = 16,
                            regions.override = 3)
                )
})
