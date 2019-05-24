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






test_that("Wide works", {
  
df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))


expect_equal(tidyr::spread( df[-4],student, A),
  Wide(df[-4],student, A) ) 
  

})


test_that("Long works", {
  
  df <- data.frame(month=rep(1:3,2),
                   student=rep(c("Amy", "Bob"), each=3),
                   A=c(9, 7, 6, 8, 6, 9),
                   B=c(6, 7, 8, 5, 6, 7))
 
df_w1 <- Wide(df[-4], student, A) 

df_a1 <- Long(Amy + Bob ~ month, df_w1, key="student", value="A") 

df_a2 <- Long(df_w1, id.vars=1, key = "student", value = "A")

df_a <- tidyr::gather(df_w1,  key = "student", value = "A", Amy, Bob)  
df_a$student<- factor(df_a$student)

expect_equal(df_a,df_a1)
expect_equal(df_a,df_a2)

df_w2 <- Wide(df, student, c(A, B))
df_w_l<-Long(list(A=c("Amy_A", "Bob_A" ), B=c("Amy_B", "Bob_B")), df_w2,
             by =  ~ month,
             key = "student",
             key.levels= c("Amy", "Bob"))



expect_equal(df,df_w_l)


})


