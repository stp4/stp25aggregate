#' Subsetting Vectors, Matrices and Data Frames
#'
#' @name Filter2
#'
#' @param x object to be subsetted.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'  dat <- data.frame(
#' nr= 1:5,
#' sex = factor(c(1, 2, 1, 2, 1), 1:3, c("m", "f", "t")),
#' treatment = c("A", "A", "B", "B", "A"),
#' m1 = c(1, NA, 2, 1, 1),
#' m2 =  c(1, 5, NA, 1, 1),
#' m3 = c(2, 1, 5, 1, 1),
#' m4 = c(3, 2,2, 2, 1),
#' m5 = c(5, 2, 3, 1, 2),
#' m6 = c(4, 1, 4, 1, 1)
#' )
#' 
#' dat <- Label(
#'   dat,
#'   sex = "Geschlecht",
#'   treatment = "Behandlung",
#'   m1 = "Cohlesterin",
#'   m2 = "Billirubin"
#' )
#'
#' dat <- tibble::as_tibble(dat)
#'
#'
#' subset2(dat, treatment == "A" & sex=="m" )
#' Select_case(dat, treatment == "A" & sex=="m", output=FALSE)
#' Filter2(dat, treatment == "A" & sex=="m")
#'
#' Drop_NA(dat, m1)
#' # tidyr::drop_na(dat,m1)
#' Drop_NA(dat)
#' Drop_case(dat, c(2:4))
#'
#'
subset2 <- function(data, ...) {
  label_data_frame(subset(data, ...), get_label(data))
}


# function (x, subset, select, drop = FALSE, ...)
# {
#   lbl<- get_label(x)
#   r <- if (missing(subset))
#     rep_len(TRUE, nrow(x))
#   else {
#     e <- substitute(subset)
#     r <- eval(e, x, parent.frame())
#     if (!is.logical(r))
#       stop("'subset' must be logical")
#     r & !is.na(r)
#   }
#   vars <- if (missing(select))
#     TRUE
#   else {
#     nl <- as.list(seq_along(x))
#     names(nl) <- names(x)
#     eval(substitute(select), nl, parent.frame())
#   }
#
#   x<- x[r, vars, drop = drop]
#
#   label_data_frame(x, lbl)
# }



#' @rdname Filter2
#' @export
#'
Select_case <- function(data, ..., output = TRUE) {
  N_in <- nrow(data)
  data <- label_data_frame(subset(data, ...), get_label(data))
  
  if (output)
    output_droped_n(N_in, nrow(data))
  data
}

#' @rdname Filter2
#' @description  \code{Filter2} copie of dplyr::filter \code{Select_case} und \code{subset2} copie of base::subset(...)
#' @export 
#'
Filter2 <- function(data, ...) {
  N_in <- nrow(data)
  d_labl <- get_label(data)
  
  label_data_frame(dplyr::filter(data, ...), d_labl)
  
}



#' @rdname Filter2
#' @description  \code{Drop_NA} entfernt Fälle mit NAs mit \code{dplyr::filter(data, ...)} wenn \code{...} leer ist ann alle also quasi na.omit
#' @export
#'
Drop_NA <- function(data, ..., output = TRUE) {
  N_in <- nrow(data)
  dataF <-  tidyr::drop_na(data, ...)
  
  # if (missing(...)) {
  #   txt <- "complete cases"
  #   f <- complete.cases(data)
  # } else {
  #   dots <- lazyeval::lazy_dots(...)
  #   txt <- as.character(unlist(dots))
  #   txt <- paste(txt[-length(txt)], collapse = ", ")
  #   f <- complete.cases(dplyr::select_(data, .dots = dots))
  # }
  # dataF <- dplyr::filter(data, f)
  if (output)
    output_droped_n(N_in, nrow(dataF))
  
  label_data_frame(dataF,
                   get_label(data))
}


#' @rdname Filter2
#' @description \code{Drop_case} entfernt Fälle arbeitet mit data[-subset, ] + label_data_frame()
#' @export
#' @param subset Ausgewählte Fälle Position als Zahl
Drop_case <- function(data,
                      subset = NULL,
                      output = TRUE) {
  if (is.null(subset)) {
    if (output)
      output_droped_n(nrow(data), txt = "no subset")
    data
  }
  else if (is.numeric(subset)) {
    N_in <- nrow(data)
    data <-  label_data_frame(data[-subset, ],
                              get_label(data))
    if (output)
      output_droped_n(N_in, nrow(data))
    data
  }
  else
    data
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
