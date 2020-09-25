context("test-dapply2")
require(stp25data)


test_that("dapply works", {
  res <- dapply2(hyper)
  expect_equal(dim(res), dim(hyper))
  expect_true(all(sapply(res, is.numeric)))
  
})


test_that("Dapply formula works", {
  
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



test_that("data.frame vs tibbel works", {


df1 <- Label(data.frame(
  month = rep(1:3, 2),
  student = rep(c("Amy", "Bob"), each = 3),
  A = c(9, 7, 6, 8, 6, 9),
  B = c(6, 7, 8, 5, 6, 7)
),
A = "Deutsch",
B = "Mathe")




df3 <- Label(
  tibble::tibble(
    month = rep(1:3, 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7)),
  A = "Deutsch",
  B = "Mathe"
)

rs1<- Dapply(
  ~ A + B,
  df1,
  fun = function(x)
    cut(x, 2)
)

rs3<-Dapply(
  ~ A + B,
  df3,
  fun = function(x)
    cut(x, 2)
)

expect_equal( 
  get_label(rs1),
  get_label(rs3)
)

expect_equal(class(df1), class(rs1))
expect_equal(class(df3), class(rs3))

})



test_that("data.frame vs formula works", {

df1 <- Label(data.frame(
  month = rep(1:3, 2),
  student = rep(c("Amy", "Bob"), each = 3),
  A = c(9, 7, 6, 8, 6, 9),
  B = c(6, 7, 8, 5, 6, 7)
),
A = "Deutsch",
B = "Mathe")

rs1 <- Dapply(~ A + B,
              df1,
              cut,
              breaks = 3,
              labels = c(1:3))

rs2 <- Dapply(
  df1,
  A,
  B,
  fun = function(x)
    cut(x, breaks = 3,  labels = c(1:3))
)


rs3 <- Dapply(
  df1,
  ~ A + B,
  fun = function(x)
    cut(x, breaks = 3,  labels = c(1:3))
)




expect_equal(rs1,rs3)
expect_equal(rs2,rs3)

})