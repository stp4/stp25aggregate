context("test-select-drop")

test_that("Drop_NA Drop_case Select_case works", {
  data <- data.frame(
    g = gl(2, 8, labels = c("Control", "Treat")),
    a = rpois(16, 5),
    b = rep(1:2, 8),
    c = rnorm(16)
  )
  
  data$d <- rep(c("", "b", "c"), 6)[1:16]
  data[2, 2] <- NA
  data <- upData2(data, labels = c(g = "Gruppe" , a = "A", b = "B"))
  
  expect_output(r1 <- Drop_NA(data, output = TRUE))
  expect_equal(dim(r1),
               c(15, 5))
  
  expect_output(r2 <- Drop_case(data, 1, output = TRUE))
  expect_equal(dim(r2),
               c(15, 5))
  
  expect_output(r3 <-
                  Select_case(data, g == "Control", output = TRUE))
  expect_equal(dim(r3),
               c(8, 5))
})


test_that("subset2 works", {
  dat <- data.frame(
    sex = factor(c(1, 2, 1, 2, 1), 1:3, c("m", "f", "t")),
    treatment = c("A", "A", "B", "B", "A"),
    m1 = c(1, NA, 2, 1, 1),
    m2 = 1:5,
    m3 = 1:5,
    m4 = 1:5,
    m5 = 1:5,
    m6 = 1:5
  )
  dat1 <- Label(
    dat,
    sex = "Geschlecht",
    treatment = "Behandlung",
    m1 = "Cohlesterin",
    m2 = "Billirubin"
  )
  
  
  subset2(dat, treatment == "A")
  dat2 <- tibble::as_tibble(dat1)
  expect_equal(dim(subset2(dat, treatment == "A"))
               , c(3, 8))
  
  expect_equal(get_label(subset2(dat1, treatment == "A")) ,
               get_label(subset2(dat2, treatment == "A")))
  
  
  
})