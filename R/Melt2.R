#' Umformen (reshape)
#'
#' Umformen von einem Breit-Format nach einem Lang-Format. Melt2 und melt2 sind
#' Erweiterungen der reshape2::melt Funktion. Intern wird \code{melt} und \code{dcast} verwendet.
#' @name Melt2
#' @rdname Melt2
#' @param x Formel ofer data.frame
#' @param ... eritere Argumente
#' @seealso \link[reshape2]{melt} und \link[reshape2]{dcast}
#'
#' @return Dataframe in Langform
#' @export
#'
#' @examples
Melt2 <- function(x, ...) {
  UseMethod("Melt2")
}


#' @rdname Melt2
#' @description Melt2.formula: Melt2(chol0+chol1+chol6+chol12~g, hyper)
#' @export
#'
#' @param data Daten

#' @param key Bezeichnung der Bezeichner-Variable default ist "variable"
#' @param value Bezeichnung der Werte-Variable default ist "value"
#' @param subset nicht verwendbar
#' @param na.action auch nicht zu verändern
#' @param X Formula-Objekt nicht ändern
#' @param id.vars  nur bei Methode data.frame zu verwenden sonst ist hier nichts zu verändern
#' @param ... weitere Argumente an melt
#' @examples
#' x<-Melt2(chol0+chol1+chol6+chol12~g , hyper)
#' aggregate( value~variable, x, mean)

Melt2.formula <-   function(x,
                            data,
                            key = "variable",
                            value = "value",
                            subset,
                            na.action =  na.pass,
                            X = stp25APA2::Formula_Data(x,
                                                        data,
                                                        subset,
                                                        na.action),
                            id.vars = X$xname,
                            ...) {
  data <- if (is.null(X$X_data))
    X$Y_data
  else
    cbind(X$X_data, X$Y_data)
  molten <- reshape2::melt(data, id.vars, ...)
  value_labels <- GetLabelOrName(X$Y_data)
  molten$variable <-
    factor(molten$variable, names(value_labels), value_labels)

  n <- length(molten)
  names(molten)[c(n - 1, n)] <- c(key, value)
  molten
}

#' @rdname Melt2
#' @description Melt2.data.frame ist wie melt zu verwenden aber die Labels werden dabei benuzt.
#' @export
#' @examples
#'
#' #-- Melt2.data.frame--
#'
#' x  <- hyper[, c("g","chol0","chol1","chol6","chol12")]
#' x  <- Melt2(x, id.vars=1)
#' # aggregate(value~variable+g, x, mean)
#'
#' # Alternative aber ohne die Labels
#' x <- hyper  %>%
#'     tidyr::gather("time", "chol", chol0:chol12) %>%
#'     dplyr::select(g, time, chol)
#'
#'
Melt2.data.frame <- function(x,
                             ...,
                             key = "variable", value = "value") {
  molten <- suppressWarnings(reshape2::melt(x, ...))

  vars <- which(names(x) %in% unique(molten$variable))
  measuer_vars <- GetLabelOrName(x[vars])

  molten$variable <-
    factor(molten$variable, names(measuer_vars), measuer_vars)
  n <- length(molten)
  names(molten)[c(n - 1, n)] <- c(key, value)
  molten
}


#' @rdname Melt2
#' @description Melt2.default gibt alles ausser die data.frames und Formeln an reshape2::melt weiter.
Melt2.default <- function(data, ...) {
  reshape2::melt(data, ...)
}

#' @rdname Melt2
#' @description melt2 ist die lazy_dots-Methode
#' @export
#' @param by Gruppierung
#' @examples
#'  head(x<-Melt2(chol0+chol1+chol6+chol12~g , hyper))
#'  # APA2(~.,x)
#'
#'  head( x<- hyper %>%  melt2(chol0,chol1,chol6,chol12, by=~g))
#'  #APA2(~.,x)
melt2 <-
  function(x,
           ...,
           by = "1") {
    measure <-
      sapply(lazyeval::lazy_dots(...), function(x) {
        as.character(x[1])
      })
    meAsNum <- grep("^[[:digit:]]", measure)
    if (length(meAsNum) != 0)
      measure[meAsNum] <- names(x[as.numeric(measure[meAsNum])])

    if (is_formula(by))
      by <- all.vars(by)

    res <- reshape2::melt (
      x,
      id.vars = by,
      measure.vars = measure,
      factorsAsStrings = FALSE
    )
    measureLev <- GetLabelOrName(x[measure])
    res$variable <- factor(res$variable, names(measureLev), measureLev)

    res
  }
