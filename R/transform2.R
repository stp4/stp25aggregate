#' @name transform2
#' @title transform2
#' @description Kopie von \link{transform} wobei die Labels
#' mit upData2 ergaenzt werden. ist
#' @param data Daten
#' @param ... alles an transform
#' @export
transform2 <- function(data, ...) {
  label_data_frame(transform(data, ...), get_label(data))
}


#' @rdname transform2
#' @export
mutate2 <- function(data, ...) {
  label_data_frame(dplyr::mutate(data, ...), get_label(data))
}



