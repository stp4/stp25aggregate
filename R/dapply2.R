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
