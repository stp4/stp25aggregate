context("test-recast2")
library(stp25data)

test_that("recast2 works", {
  dat <- data.frame(
    Group = gl(2, 4, labels = c("Control", "Treat")),
    X = c(1, 1, 1, 1,   2, 2, 2, 2),
    Y = c(3, 3, 3, 3,   4, 4, 4, 4),
    Z = c(5, 5, 5, 5,   6, 6, 6, 6)
  )
  
  expect_equivalent(
    Recast2(X + Y + Z ~ Group, dat, function(x)
      round(mean(x), 2)),
    
    data.frame(
      Group  = c("Control", "Control", "Control", "Treat", "Treat", "Treat"),
      variable = c("X", "Y", "Z" , "X", "Y", "Z"),
      value  = c(1, 3, 5, 2, 4, 6)
    )
  )
  
  hyper1 <- hyper[, c("g", "chol0", "chol1", "chol6", "chol12")]
  expect_warning(hyper_long <- reshape2::melt(hyper1, id.vars = 1))
  
    expect_equal(
    aggregate(value ~ variable + g, hyper_long, mean)$value,
    Recast2(chol0 + chol1 + chol6 + chol12 ~ g, hyper, mean)$value
  )
  
})




test_that("Summarise works", {
  df <- data.frame(
    month = rep(1:3, 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7)
  )
  mean3 <- function(x)
    round(mean(x), 1)
  
  x1 <-
    Summarise(A + B ~ student, df, mean3, key = "group", value = "cbc")
  
  expect_equal(x1$cbc, c(7.3, 7.0, 7.7, 6.0))
  
  x2 <- Summarise(A + B ~ student,
                  df,
                  mean3,
                  formula = variable ~ student,
                  margins = TRUE)
  expect_equal(names(x2), c("variable" , "Amy" , "Bob" , "(all)"))
  
  x3 <- Summarise(A + B ~ student, df, mean3,  margins = TRUE)
  
  expect_equal(x3$value,  c(7.3, 7.0, 7.7, 6.0, 7.5, 6.5))
  
  
})