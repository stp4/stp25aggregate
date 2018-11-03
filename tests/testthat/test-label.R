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
