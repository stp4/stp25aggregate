context("test-dapply2")
require(stp25data)


test_that("dapply works", {
  res <- dapply2(hyper)
  expect_equal(dim(res), dim(hyper))
  expect_true(all(sapply(res, is.numeric)))
  
})


test_that("dapply works", {
  
df <- data.frame(
  month = rep(1:3, 2),
  student = rep(c("Amy", "Bob"), each = 3),
  A = c(9, 7, 6, 8, 6, 9),
  B = c(6, 7, 8, 5, 6, 7)
)

d1<- Dapply(~A+B, df, fun=function(x) cut(x, 2))

df$A <- cut(df$A, 2)
df$B <- cut(df$B, 2)  
 
df<- Label(df, A="A", B="B") 
expect_equal(df, d1)

})


