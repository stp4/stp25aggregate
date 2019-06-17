context("test-label")

test_that("Label and GetLabelOrName works", {
  require(stp25data)
  x <-
    Label(
      hyper,
      nr = "Id",
      med = "Medikament vor OB",
      g = "Geschlecht",
      a = "Alter bei OB"
    )
  
  expect_equal(
    GetLabelOrName(x[1:4]),
    c(
      nr = "Id",
      med = "Medikament vor OB",
      g = "Geschlecht",
      a = "Alter bei OB"
    )
  )
})




test_that("Label tibbel-data.frame", {
  dat1 <- data.frame(
    sex = factor(c(1, 2, 1, 2, 1), 1:3, c("m", "f", "t")),
    treatment = c("A", "A", "B", "B", "A"),
    m = c(1, NA, 2, 1, 1)
  )
  
  dat2 <-
    tibble::tibble(
      sex = factor(c(1, 2, 1, 2, 1), 1:3, c("m", "f", "t")),
      treatment = c("A", "A", "B", "B", "A"),
      m = c(1, NA, 2, 1, 1)
    )
  expect_equal(get_label(dat1),
               get_label(dat2))
  
  
  expect_equal(get_label(Label(dat1, sex = "Gechlecht", dumm = "Hallo")),
               get_label(Label(dat2, sex = "Gechlecht", dumm = "Hallo")))
  
  
})



