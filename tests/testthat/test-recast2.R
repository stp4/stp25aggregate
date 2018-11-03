context("test-recast2")
library(stp25data)

test_that("mean works", {
  dat <- data.frame(
    Group =gl(2, 4, labels = c("Control", "Treat")),
    X = c(1,1,1,1,   2,2,2,2 ),
    Y = c(3,3,3,3,   4,4,4,4),
    Z = c(5,5,5,5,   6,6,6,6) 
  )
  
  expect_equivalent(
  Recast2(X+Y+Z~Group, dat, function(x) round(mean(x),2)),
  
  data.frame(
    Group  = c("Control", "Control", "Control", "Treat", "Treat", "Treat"),
    variable = c("X", "Y", "Z" , "X", "Y", "Z"),
    value  = c(1, 3, 5, 2, 4, 6)
  )
  )
  
  
  
  hyper1<-hyper[, c("g","chol0","chol1","chol6","chol12")]
  hyper_long<- reshape2::melt(hyper1, id.vars=1)
  
 
  expect_equal(
  aggregate( value~variable+g, hyper_long, mean)$value,
  Recast2(chol0+chol1+chol6+chol12~g, hyper, mean)$value) 
  
  
  
})
