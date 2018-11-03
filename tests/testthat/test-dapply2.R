context("test-dapply2")
require(stp25data)
test_that("dapply works", {
  res <- dapply2(hyper)
  expect_equal(dim(res), dim(hyper))
  expect_true(all(sapply(res, is.numeric)))
  
})
