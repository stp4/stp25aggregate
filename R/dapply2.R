# Ueberarbeitet am  "Sun Apr 14 10:39:13 2019"




#' Apply mit Hmisc labels
#'
#' @description  Mischung usd der \code{llply} und \code{data.frame} -Funktion.
#' Erwartet einen data.frame und die Daten werdentransformiert.
#' Wenn keine Funktion uebergeben wird wird der Datensatz zu Zahlen transformiert.
#' @param .data  Data.frame
#' @param fun   funktion
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @param ... Weitere Argumente an llply
#' @return upData dataframe
#' @author Wolfgang Peter
#' @export
#' @examples
#'
#'
dapply2 <- function (.data,
                     fun = function(x)
                       as.numeric(x),
                     stringsAsFactors = default.stringsAsFactors(),
                     ...) {
  if (inherits(.data, "tbl_df"))
    label_data_frame(dplyr::tbl_df(plyr::llply(.data, fun, ...)),
                     GetLabelOrName(.data))
  else
    label_data_frame(data.frame(plyr::llply(.data, fun, ...),
                                stringsAsFactors=stringsAsFactors),
                     GetLabelOrName(.data))
}


#' Make New Variables
#' 
#' Apply mit formula
#'
#' @param x Objekt
#' @param ... an dapply2
#'
#' @return dataframe
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(stp25aggregate)
#' library(stp25data)
#' res <- Dapply(~ a  +  gr  + gew+chol0+chol1+chol6, hyper, fun=function(x) cut(x,3))
#' head(res)
#'
#'
Dapply <- function(x, ...) {
  UseMethod("Dapply")
}


#' @rdname dapply2
#' @export
Dapply.formula <- function(x,
                          .data,
                          fun = function(x)
                            as.numeric(x),
                          stringsAsFactors = default.stringsAsFactors()) {
  X <- stp25formula::prepare_data2(x, .data)
  apply_data <- dapply2(X$data[X$measure.vars], fun)
  
  .data[, X$measure.vars] <- apply_data
  .data
}

#' @rdname dapply2
#' @export
Dapply.data.frame <- function(.data,
                             ...,
                             fun = function(x)
                               as.numeric(x),
                             stringsAsFactors = default.stringsAsFactors()) {
  X <- stp25formula::prepare_data2(.data, ...)
  apply_data <- dapply2(X$data[X$measure.vars], fun)
  
  .data[, X$measure.vars] <- apply_data
  .data
}

