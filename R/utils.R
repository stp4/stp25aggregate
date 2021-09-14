#
# in Recast2 verwendet
#
all_identical2 <- function(data) {
  if (ncol(data) < 2) {
    TRUE
  }
  else{
    xs <-
      sapply(data, function(x)
        if (is.numeric(x))
          "numeric"
        else if (is.factor(x))
          "factor"
        else
          NA)
    if (length(xs) <= 1)
      return(TRUE)
    for (i in seq(2, length(xs))) {
      if (!identical(xs[[1]], xs[[i]]))
        return(FALSE)
    }
    TRUE
  }
}

isFALSE <- function(x){identical(FALSE, x )}

is_formula2<- function (x)
  inherits(x, "formula")



