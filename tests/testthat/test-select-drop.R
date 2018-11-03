context("test-select-drop")

test_that("Drop_NA Drop_case Select_case works", {
  data<- data.frame(g= gl(2, 8, labels = c("Control", "Treat")),
                    a=rpois(16, 5),
                    b=rep(1:2,8),
                    c=rnorm(16))
  
  data$d<-rep(c("", "b", "c"), 6)[1:16]
  data[2,2]<- NA
  data<-upData2(data, labels = c(g="Gruppe" , a="A", b="B") ) 

  expect_equal( dim(Drop_NA(data)),
  c(15,5))
  expect_equal(   dim(Drop_case(data, 1)),
  c(15,5))
  expect_equal( dim(Select_case(data, g=="Control")),
  c(8,5))
})
