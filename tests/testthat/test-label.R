context("test-label")

test_that("Label and GetLabelOrName works", {
   
  
  df <- data.frame(
    BMI=c(1,2,3,1,2,3),
    WHtR= gl(2,3, labels =c("Amy", "Bob")),
    WHtR_1=c(9,7,6,8,6,9),
    bildprof=c(6,7,8,5,6,7)
  )
  
  DF<-
    Label(df, BMI = "Body-Mass-Index",
          WHtR =  "Waist-Height-Ratio",
          WHtR_1 ="Waist-Height-Ratio"
    )
  
  DF$BMI<- units::set_units(DF$BMI, kg/m2)
  expect_equal(
    get_label(DF),
    c(
      BMI  = "Body-Mass-Index",
      WHtR  = "Waist-Height-Ratio",
      WHtR_1 = "Waist-Height-Ratio",
      bildprof = "bildprof"
    ))
  
  
  expect_equal(
    get_label(DF, include.units=TRUE),
    c(
      BMI  = "Body-Mass-Index [kg/m2]",
      WHtR  = "Waist-Height-Ratio",
      WHtR_1 = "Waist-Height-Ratio",
      bildprof = "bildprof"
    ))
  
  
  
  DF<- set_label(DF, c(bildprof = "Bildungsprofil"))
  
  expect_equal(
    get_label(DF),
    c(
      BMI  = "Body-Mass-Index",
      WHtR  = "Waist-Height-Ratio",
      WHtR_1 = "Waist-Height-Ratio",
      bildprof = "Bildungsprofil"
    ))
  
 
  DF<- delet_labels(DF)
  
  get_label(DF)
  
  
  expect_equal(
    get_label(DF),
    c(
      BMI  = "BMI",
      WHtR  = "WHtR",
      WHtR_1 = "WHtR_1",
      bildprof = "bildprof"
    ))
  
  
})




test_that("Label tibbel-data.frame", {
  df1 <- data.frame(
    BMI=c(1,2,3,1,2,3),
    WHtR= gl(2,3, labels =c("Amy", "Bob")),
    WHtR_1=c(9,7,6,8,6,9),
    bildprof=c(6,7,8,5,6,7)
  )
  
  DF1<-
    Label(df1, BMI = "Body-Mass-Index",
          WHtR =  "Waist-Height-Ratio",
          WHtR_1 ="Waist-Height-Ratio"
    )
  
  
  df2 <- tibble::tibble(
    BMI=c(1,2,3,1,2,3),
    WHtR= gl(2,3, labels =c("Amy", "Bob")),
    WHtR_1=c(9,7,6,8,6,9),
    bildprof=c(6,7,8,5,6,7)
  )
  
  DF2<-
    Label(df2, BMI = "Body-Mass-Index",
          WHtR =  "Waist-Height-Ratio",
          WHtR_1 ="Waist-Height-Ratio"
    )
 
  expect_equal(
    get_label(DF1),
    get_label(DF2))
 
 
  
  
})



