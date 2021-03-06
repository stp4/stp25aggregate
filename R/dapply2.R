#' Dapply 
#' 
#' Dapply, dapply2 ist  plyr::llply()
#' 
#' @param x Objekt
#' @param ... Weitere Argumente an llply
#' @return  data.frame
#' @export
#' @examples
#'  
#' df1 <- Label(data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' ),
#' A = "Deutsch",
#' B = "Mathe")
#' 
#' rs1 <- Dapply(~ A + B,
#'               df1,
#'               cut,
#'               breaks = 3,
#'               labels = c(1:3))
#'               
Dapply <- function(x, ...) {
  UseMethod("Dapply")
}


#' @rdname Dapply
#' @param data  Data.frame
#' @param fun   funktion function(x) as.numeric(x)
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @export
#' 
dapply2 <- function (data,
                     fun = function(x) as.numeric(x),
                     stringsAsFactors = default.stringsAsFactors(),
                     ...) {
  if (tibble::is_tibble(data))
    label_data_frame(tibble::as_tibble(plyr::llply(data, fun, ...)),
                     get_label(data))
  else
    label_data_frame(data.frame(plyr::llply(data, fun, ...),
                                stringsAsFactors=stringsAsFactors),
                     get_label(data))
}


#' @rdname Dapply
#' @export
Dapply.formula <- function(x,
                          data,
                          fun = function(xx){as.numeric(xx)},
                          stringsAsFactors = default.stringsAsFactors(),
                          ...) {
  X <- stp25formula::prepare_data2(x, data)
  
  apply_data <- dapply2(X$data[X$measure.vars], 
                        fun=fun, 
                        stringsAsFactors=stringsAsFactors,
                        ...)
  
  data[, X$measure.vars] <- apply_data
  data
}


#' @rdname Dapply
#' @export
Dapply.data.frame <- function(data,
                             ...,
                             fun = function(x)
                               as.numeric(x),
                             stringsAsFactors = default.stringsAsFactors()) {
 
  X <- stp25formula::prepare_data2(data, ...)
  apply_data <- dapply2(X$data[X$measure.vars], fun)
  
  data[, X$measure.vars] <- apply_data
  data

}

