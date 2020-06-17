#' @rdname Long
#' @return Dataframe in Langfor
#' @export
Melt2 <- function(x, ...) {
  UseMethod("Melt2")
}


#' @rdname Long
#' @description Melt2.formula: Melt2(chol0+chol1+chol6+chol12~g, hyper)
#' @export
#'
#' @param data Daten

#' @param key Bezeichnung der Bezeichner-Variable default ist "variable"
#' @param value Bezeichnung der Werte-Variable default ist "value"
#' @param na.action auch nicht zu veraendern
#' @param X Formula-Objekt nicht ändern
#' @param id.vars  nur bei Methode data.frame zu verwenden sonst ist hier nichts zu veraendern
#' @param ... weitere Argumente an melt
#' @examples
#' x<-Melt2(chol0+chol1+chol6+chol12~g , hyper)
#' aggregate( value~variable, x, mean)

Melt2.formula <-   function(x,
                            data,
                            key = "variable",
                            value = "value",
                            na.action =  na.pass,
                            X = stp25formula::prepare_data2(x, data,  na.action = na.action),
                            id.vars = X$group.vars,
                            ...) {
  
   # message("In Melt2.formula" )
  molten <- reshape2::melt(X$data, id.vars, ...)

  value_labels <- X$row_name
  
  molten$variable <-
    factor(molten$variable, names(value_labels), value_labels)

  n <- length(molten)
  names(molten)[c(n - 1, n)] <- c(key, value)
  molten
}

#' @rdname Long
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
                             key = "variable", 
                             value = "value") {
 
  molten <- suppressWarnings(reshape2::melt(x, ...))
  
  vars <- which(names(x) %in% unique(molten$variable))
  measuer_vars <- get_label(x[vars])
  
  molten$variable <-
    factor(molten$variable, names(measuer_vars), measuer_vars)
  n <- length(molten)
  names(molten)[c(n - 1, n)] <- c(key, value)
  molten
}


#' @rdname Long
#' @description Melt2.default gibt alles ausser die data.frames und Formeln an reshape2::melt weiter.
Melt2.default <- function(data, ...,
                          key = "variable", 
                          value = "value") {
 
  reshape2::melt(data, ...)
}

#' @rdname Long
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
           by = NULL,
           key = "variable", 
           value = "value") {
  
    measure <-
      sapply(lazyeval::lazy_dots(...), function(x) {
        as.character(x[1])
      })

    meAsNum <- grep("^[[:digit:]]", measure)
    if (length(meAsNum) != 0)
      measure[meAsNum] <- names(x[as.numeric(measure[meAsNum])])

    if (is_formula2(by))
      by <- all.vars(by)
    
    if(length(measure)==0){
      if( is.null(by)) measure<- names(x)
      else  measure <- setdiff(names(x), by)
    }

    molten <- reshape2::melt (
      x,
      id.vars = by,
      measure.vars = measure,
      factorsAsStrings = FALSE
    )
    measureLev <- get_label(x[measure])
    molten$variable <- factor(molten$variable, names(measureLev), measureLev)
    n <- length(molten)
    names(molten)[c(n - 1, n)] <- c(key, value)
    molten
  }
