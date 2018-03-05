context("plot.nexus")
test_that("Nexus file can be parsed", {
  filename <- 'test-parse-nexus.nexus'
  read <- read.characters(filename)
  expect_equal(192, ncol(read))
  expect_equal(80, nrow(read))
  expect_equal("Wiwaxia", rownames(read)[4])
  expect_equal("(01)", as.character(read[1, 27]))
})

