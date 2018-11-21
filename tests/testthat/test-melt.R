context("test-melt")
require(stp25data)
test_that("Melt2 und melt2 works", {
  expect_warning(x1 <-
                   Melt2(chol0 + chol1 + chol6 + chol12 ~ g , hyper))
  expect_equal(names(x1),
               c("g", "variable", "value"))
  
  x  <- hyper[, c("g", "chol0", "chol1", "chol6", "chol12")]
  x2 <- Melt2(x, id.vars = 1)
  
  expect_warning(x3 <-
                   melt2(hyper, chol0, chol1, chol6, chol12, by =  ~ g))
  
  expect_equal(
    aggregate(value ~ variable, x1, mean)$value,
    aggregate(value ~ variable, x2, mean)$value
  )
  expect_equal(
    aggregate(value ~ variable, x1, mean)$value,
    aggregate(value ~ variable, x3, mean)$value
  )
  expect_equivalent(round(aggregate(value ~ variable, x3, mean)$value, 4),
                    c(237.2701, 239.1954, 236.5862, 233.0977))
})


