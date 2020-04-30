context("RunInapp")

## Test
test_that("RunInapp works", {

    ## Sanitizing
    expect_error(runInapp(remote = "blue"))

    ## Expect running (but not running)
    expect_warning(msg <- capture_messages(expect_error(runInapp(remote = TRUE, ref = "doesntexist"))))
    expect_equal(msg, "Downloading https://github.com/TGuillerme/Inapp/archive/doesntexist.tar.gz\n")

    ## Expect running (but not running)
    expect_error(runInapp(remote = FALSE, path = "dummy"))
})
