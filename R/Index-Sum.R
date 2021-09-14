#' Summen-Index
#'  
#'  Summen Index eine Summenfunktion mit der Erweiterung zum Umcodieren
#' @param return.index TRUE/FALSE index oder Daten
#' @return Vektor
#' @export
#' 
Index <- function(x,
                  revcoded = FALSE,
                  fun = "mean",
                  na.rm = TRUE,
                  digits = 4,
                  max.level = NA,
                  min.level = NA,
                  return.index = TRUE,
                  ...) {
  if (!all(apply(x, 2, function(objekt) {
    class(objekt) == "numeric" || class(objekt) == "integer"
  }))) {
    if (any(unlist(
      lapply(x, function(objekt)
        class(objekt) == "factor" ||
        class(objekt) == "labelled")
    ))) {
      cat("\nKonvertiere Faktoren zu Zahlen!\n\n")
      x <- data.frame(lapply(x, as.numeric))
    } else {
      cat(
        "\n",
        "Falsches Datenformat (Numeric oder Faktor ist erlaubt)",
        "\n",
        apply(x, 2, function(objekt)
          class(objekt)),
        "\n\n"
      )
      return(rep(NA, nrow(x)))
    }
  }
  if (!is.logical(revcoded)) {
    cat("\n", "Umcodieren ", paste(revcoded, collapse = ", "), "\n")
    print(head(x))
    x <- Umcodieren(x, revcoded, max.level, min.level)
    print(head(x))
  }
  index <- switch(
    fun,
    mean = round(rowMeans(x, na.rm = na.rm), digits),
    sum =  round(rowSums(x, na.rm = na.rm), digits),
    rep(NA, nrow(x))
  )
  
  if (return.index)
    return(index)
  else
    return(list(data = x, index = index))
  
}


#' @rdname Index
#' @export
Sum2 <- function(...,
                 revcoded = FALSE,
                 fun = "mean",
                 na.rm = TRUE,
                 digits = 4,
                 max.level = NA,
                 min.level = NA) {
  dat <- stp25tools::fix_to_df(list(...))
  Index(dat, revcoded =revcoded, 
        fun =fun,
        na.rm =na.rm,
        digits=digits,
        max.level=max.level,
        min.level=min.level,
        return.index=TRUE)
  
}


#' Umcodieren
#'
#' @noRd
Umcodieren <- function(x,
                       revcoded,
                       max.level = NA,
                       min.level = NA) {
  if (is.na(max.level))
    max.level <- max(x, na.rm = TRUE)
  if (is.na(min.level))
    min.level <- min(x, na.rm = TRUE)
  mytempdata <- x[, revcoded]
  
  if (is.numeric(mytempdata))
    x[, revcoded] <- max.level + min.level - mytempdata
  else
    x[, revcoded] <-
    apply(mytempdata, 2, function(item)
      max.level + min.level - item)
  return(x)
}



#' Transformieren zu numeric
#'
#' @noRd
transform_to_numeric <- function(data, data_range) {
  #data2<- na.omit(data)
  lvls <- get_label(data)
  
  objects <-
    sapply(data, function(x)
      if (is.factor(x))
        "factor"
      else if (is.numeric(x))
        "numeric"
      else
        "unknown")
  if (all(objects == "numeric"))
    data_range <- range(data, na.rm = T)
  else if (all(objects == "factor")) {
    data <- data.frame(sapply(data, as.numeric))
    data_range <- range(data, na.rm = T)
  }
  else {
    cat("\n",
        "Falsches Datenformat (Numeric oder Faktor ist erlaubt)",
        "\n")
    # print(objects)
    data <- sapply(data, as.numeric)
    data_range <- range(data, na.rm = T)
  }
  
  list(data = data,
       range = data_range,
       labels = lvls)
  
}
