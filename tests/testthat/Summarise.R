context("test-Summarise")
#require(stp25data)
test_that("Summarise  works", {
  
  
  
  mean3 <- function(x)
    round(mean(x, na.rm = TRUE), 2)
  df <- data.frame(
    month = rep(1:3, 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7)
  )
  expect_equivalent(
  Summarise(A + B ~ student, df),
  data.frame(student= c( "Amy", "Amy", "Bob", "Bob"),
             variable= factor(c("A", "B", "A", "B")),
             value=c(3,3,3,3)))
  
  # student variable value
  # 1     Amy        A     3
  # 3     Amy        B     3
  # 2     Bob        A     3
  # 4     Bob        B     3
  expect_equivalent(
  Summarise(
    df,
    A,  B,
    by =  ~  month,
    fun = mean3,
    formula = month ~ variable,
    margins = TRUE
  ),
  tibble::tibble(
    month= c("1","2","3","Total"),
    A=c(8.5,6.5,7.5,7.5),
    B=c(5.5,6.5,7.5,6.5)
  )
  )
  # # A tibble: 4 x 3
  # month     A     B
  # <chr> <dbl> <dbl>
  #   1 1       8.5   5.5
  # 2 2       6.5   6.5
  # 3 3       7.5   7.5
  # 4 Total   7.5   6.5
  
 
  # Summarise(
  #   chol0 + chol1 + chol6 + chol12 ~ g,
  #   hyper,
  #   fun = mean,
  #   key = "Zeit",
  #   value = "Cholesterin"
  # )
  # 
  # 
  #                        Zeit Cholesterin
  # 1 männlich    Cholesterin, Ausgangswert    228.9492
  # 3 männlich    Cholesterin, nach 1 Monat    233.0508
  # 5 männlich  Cholesterin, nach 6 Monaten    233.5932
  # 7 männlich Cholesterin, nach 12 Monaten    227.2203
  # 2 weiblich    Cholesterin, Ausgangswert    241.5391
  # 4 weiblich    Cholesterin, nach 1 Monat    242.3478
  # 6 weiblich  Cholesterin, nach 6 Monaten    238.1217
  # 8 weiblich Cholesterin, nach 12 Monaten    236.1130
   
#  expect_equal
 
})





 

