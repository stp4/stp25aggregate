#
#' @rdname CleanUp
#' @description Filter und Subset \code{Drop_NA} entfernt Fälle mit NAs mit \code{dplyr::filter}
#' @export
#' @examples
#' data<- data.frame(g= gl(2, 8, labels = c("Control", "Treat")),
#' a=rpois(16, 5),
#' b=rep(1:2,8),
#' c=rnorm(16))
#'
#' data$d<-rep(c("", "b", "c"), 6)[1:16]
#' data[2,2]<- NA
#' str(data<-upData2(data, labels = c(g="Gruppe" , a="A", b="B") ))
#' str(Drop_NA(data))
Drop_NA <- function(data, ...) {
  N_in <- nrow(data)
  if (missing(...)) {
    txt <- "complete cases"
    f <- complete.cases(data)
  } else {
    dots <- lazyeval::lazy_dots(...)
    txt <- as.character(unlist(dots))
    txt <- paste(txt[-length(txt)], collapse = ", ")
    f <- complete.cases(dplyr::select_(data, .dots = dots))
  }
  dataF <- dplyr::filter(data, f)
  output_droped_n(N_in, nrow(dataF), txt)
  
  label_data_frame(dataF,
                   GetLabelOrName(data))
}


output_droped_n <- function(N = 0,
                            n1 = N,
                            txt = "") {
  stp25output::HTML_B(paste0("Filter: ", 
                             txt, 
                             " (N =", 
                             N,  "/", 
                             n1, ")"))
}


#' @rdname CleanUp
#' @description \code{Drop_case} entfernt Fälle arbeitet auch wie subset()
#' @export
#' @param subset Ausgewählte Fälle
#' @examples
#'
#' str(Drop_case(data, 1))
#'
#'#-  Aber das geht auch
#' APA2(~., Drop_case(data, 1)  )
#'
#' APA2(~., dplyr::slice(data, -1))
#'
Drop_case <- function(data, subset = NULL, ...) {
  if (is.null(subset)) {
    output_droped_n(nrow(data), txt = "no subset")
    data
  }
  else if (is.numeric(subset)) {
    N_in <- nrow(data)
    data <-  label_data_frame(data[-subset, ],
                              GetLabelOrName(data))
    output_droped_n(N_in, nrow(data))
    data
  }
  else
    data
}


#' @rdname CleanUp
#' @description \code{Drop_case} entfernt Fälle arbeitet auch wie subset()
#' @export
#' @examples
#'
#' #' str(Select_case(data, g=="Control"))
#'
Select_case <- function(data,  ...) {
  N_in <- nrow(data)
  data <- label_data_frame(subset(data, ...), GetLabelOrName(data))
  output_droped_n(N_in, nrow(data))
  data
}
