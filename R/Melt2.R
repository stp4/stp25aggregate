# Ueberarbeitet am  "Sun Apr 14 10:39:13 2019"
# neu ist die Funktion Long




#' Umformen (reshape)
#'
#' Umformen von einem Breit-Format nach einem Lang-Format. Melt2 und melt2 sind
#' Erweiterungen der reshape2::melt Funktion. Intern wird  melt und dcast verwendet.
#' @param x Objekt kann Formel oder data.frame sein
#' @param ... weitere Argument 
#'
#' @return data.frame
#' @export
 
Long <- function(x, ...) {
  UseMethod("Long")
}

#' @rdname Long
#' @export
Long.formula <- function(x, .data, ...) {
  is_tbl <- dplyr::is.tbl(.data)
  molten <- Melt2.formula(x, .data, ...)
  if (is_tbl) tibble::as_tibble(molten)
  else molten
}

#' @rdname Long
#' @export
Long.data.frame <- function(.data, ..., id.vars = NULL) {
  is_tbl <- dplyr::is.tbl(.data)
  if (is.null(id.vars))
    molten <- melt2(.data, ...)
  else
    molten <- Melt2.data.frame(.data, id.vars=id.vars, ...)
  
  if (is_tbl) tibble::as_tibble(molten)
  else molten
  
}


 
#' @param key.levels wenn value gesetzt wird dann 1:nlevels
#'
#' @rdname Long
#' @export
Long.list <- function(x,
                      .data,
                      by = NULL,
                      key = NULL,
                      value = NULL,
                      key.levels = NULL,
                      ...) {
  if (!all(lengths(x)[1] == lengths(x)))
    stop("Die liste mus gleich lang sein!")
  is_tbl <- dplyr::is.tbl(.data)
  
  first_var <- x[[1]]
  if (!is.null(by)) {
    if (is_formula2(by))
      by <- all.vars(by)
    dat <- .data[c(by, first_var)]
    
  } else{
    dat <- .data[first_var]
  }
  molten <- melt2(dat, by = by, value = names(x)[1],  ...)
  
  if (!is.null(key)) {
    if (is.null(key.levels))
      key.levels <- 1:nlevels(molten$variable)
    levels(molten$variable) <-  key.levels
    names(molten)[(length(molten) - 1)] <- key
  }
  
  if (length(x) > 1)
    for (i in names(x)[-1]) {
      next_var <- x[[i]]
      dat <- .data[next_var]
      next_molten <- melt2(dat, value = i,  ...)
      if (!is.null(key)) {
        next_molten <-  next_molten[-1]
      }
      
      molten <- cbind(molten, next_molten)
    }
  
  
  if (is_tbl) tibble::as_tibble(molten)
  else molten
}





#' @rdname Long
#' @description 
#' Transformiert von Long nach Wide
#' Quelle: https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
#'
#' WArnung: die Funktion ist experimental
#' @export
#'
#' @examples
#'   suppressPackageStartupMessages(library(tidyverse))
#' 
#' df <- data.frame(month=rep(1:3,2),
#'                  student=rep(c("Amy", "Bob"), each=3),
#'                  A=c(9, 7, 6, 8, 6, 9),
#'                  B=c(6, 7, 8, 5, 6, 7))
#' df %>% Wide(student, c(A, B))
#' 
#' df[-4] %>% tidyr::spread(student, A)
#' df[-4] %>% Wide(student, A)
#' 
#' 
#' df_widw %>% Long( Amy_A, Amy_B, Bob_A, Bob_B, by=~month)
#' Long(list( A=c("Amy_A", "Bob_A"), B=c( "Amy_B", "Bob_B")),
#'      df_widw,
#'      by=~month,
#'      key = "student",
#'      key.level=c("Amy", "Bob")
#'      )
#'  
#'  t1 <- df %>%
#'  tidyr::gather(variable, value, -(month:student)) %>%
#'    tidyr::unite(temp, student, variable) %>%
#'     tidyr::spread(temp, value)

Wide <- function(.data, key, value) {
  
 # if (!dplyr::is.tbl(.data))
  #  .data <- tibble::as_tibble(.data)
    
   
  
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  
  # test length value
  if (length(rlang::quo_get_expr(valueq)) == 1) {
    tidyr::spread(.data, !!keyq, !!valueq)
  } else{
    s <- rlang::quos(!!valueq)
    
    tidyr::spread(
      tidyr::unite(
        tidyr::gather(.data, variable, value, !!!s),
      temp, !!keyq, variable),
    temp, value
    )
  }
}



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
#' @param subset nicht verwendbar
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
                            subset,
                            na.action =  na.pass,
                            X = stp25formula::prepare_data2(x,
                                                        data,
                                                        subset,
                                                        na.action),
                            id.vars = X$group.vars,
                            ...) {
 
 
  
  molten <- reshape2::melt(X$data, id.vars, ...)
  
  value_labels <- GetLabelOrName(X$data[X$measure.vars])
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
  measuer_vars <- GetLabelOrName(x[vars])
  
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
    measureLev <- GetLabelOrName(x[measure])
    molten$variable <- factor(molten$variable, names(measureLev), measureLev)
    n <- length(molten)
    names(molten)[c(n - 1, n)] <- c(key, value)
    molten
  }
