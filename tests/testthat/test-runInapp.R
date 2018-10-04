context("RunInapp")

## Test
test_that("RunInapp works", {

    ## Sanitizing
    expect_error(runInapp(remote = "blue"))
})
