#' Summarise Data
#'
#' @description  Erweiterung der recast-Funktion. Erwartet einen data.frame
#' mit \code{label} und verwendet die \code{melt} und \code{dcast}  Funftionen
#' vom Packet library(reshape2).
#' @param Formula   Formula-Objekt
#' @param data   Daten
#' @param fun Agregat Funktion
#' @param key,value Names of new key and value columns, as strings
#' @param na.action  fuer Formula na.action = na.omit, na.pass, na.exclude, na.fail
#' @param formula  zum vertauchen von Spalten Argument an dcast
#' @param labels sollen Hmisc levels verwendet werden
#' @param margins Gesamtwert bei TRUE eigene Funktion sonst dcast
#' @param measure.var Name der Gemessenen Variable
#' @param X Formula-Objekt
#' @param ... recast Arguments \link[reshape2]{recast}
#' @return reshape objek
#' @seealso \link[reshape2]{recast}
#' @author Wolfgang Peter
#' @export
#' @examples
#'
#' #library(tidyverse)
#' #library(stp25aggregate)
#' #library(stp25data)
#'
#' hyper1<-hyper[, c("g","chol0","chol1","chol6","chol12")]
#' hyper_long<- Melt2(hyper1, id.vars=1)
#' aggregate( value~variable, hyper_long, mean)
#'
#' hyper_long<-Melt2(chol0+chol1+chol6+chol12~g,
#'                   hyper, "Zeit", "Cholesterin")
#' #-- Spread + aggragate
#' aggregate(Cholesterin~Zeit+g, hyper_long, mean) %>%
#'   spread(g, Cholesterin)
#'
#' #- das gleiche wie aggragate
#' hyper_long %>% group_by(Zeit, g) %>%
#'   summarise(Cholesterin=mean(Cholesterin)) %>%
#'   spread(g, Cholesterin)
#'
#' #-- Gather das gleiche wie oben aber ohne die Labels
#' hyper  %>%
#'   tidyr::gather("time", "chol", chol0:chol12) %>%
#'   dplyr::select(g, time, chol)
#'
#' #-- Recast2 das gleiche wie oben
#' Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
#'         "Zeit", "Cholesterin") %>%
#'   spread(g, Cholesterin)
#'
#' #-- Recast2 das gleiche wie oben nur ohne  spread
#' Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
#'         formula=variable~g)
#'
#'
#' Recast2(chol0+chol1+chol6+chol12~g, hyper,
#'         mean,formula=variable~g, margins=TRUE)
#'
Recast2 <- function(Formula,
                    data,
                    fun = NULL,
                    key = "variable", value = "value",
               
                    na.action = na.pass,
                    X = stp25formula::Formula_Data(Formula, data, 
                                                   na.action=na.action),
                    id.vars = X$xname,
                    measure.var,
                    formula = NULL,
                    labels = TRUE,
                    ### wegen der Reihenfolge any(Hmisc::label(X$Y_data)!=""),
                    margins = FALSE,
                    # bei variablename weitergabe an dcast sonst droeel
                    margins_name = "gesamt",

                    ...) {
  default_formula <- paste(paste(c(id.vars, "variable"), collapse = "+") , "~'value'")
  mynames <- c(X$xname, X$yname)


  if ("variable" %in% mynames | "value" %in% mynames)
    warning(
      "Die Variablen-Namen variable oder value sind nicht erlaubt in Recast2\nDie Namen waren: ",
      paste(mynames, collapse = ", ")
    )

  if (is.null(formula)) {
    ad_key_value <- TRUE
    formula <- default_formula
    default_formula <- TRUE
  } else{
    ad_key_value <- FALSE
    default_formula <- FALSE
  }

  if (!all_identical2(X$Y_data)) {
    print(head(X$Y_data))
    cat("\nNicht berechenbar da falsche Skalenniveau (Faktor und Zahlen gemischt!)\n")
    return(NULL)
  }
  data <- if (is.null(X$X_data))
    X$Y_data
  else
    cbind(X$X_data, X$Y_data)
  value_labels <- NULL
  group_labels <- NULL

  molten <-
    suppressWarnings(reshape2::melt(data, id.vars, measure.var))
  if (labels) {
    value_labels <- get_label(X$Y_data)
    group_labels <- get_label(X$X_data)
    # --  wegen doppelter Labels
    value_labels2 <-
      if (length(unique(value_labels)) != length(value_labels))
        paste0("(", 1:length(value_labels), ") ", value_labels)
    else
      value_labels
    molten$variable <-
      factor(molten$variable, names(value_labels), value_labels2)
  }
  #-------------------------------------------------------------------------------
  if (default_formula & (isFALSE(margins))) {
    result <-  reshape2::dcast(molten, formula, fun, ...)
    if (labels & (!is.null(group_labels)))
       result <-   set_label(result, labels = group_labels)

  } else if (default_formula &  isTRUE(margins)) {
    #- Workaraound  fuer margins weil die nicht das gewuensche Ergebniss liefern
    #- orginal dcast produzertz ueberall die margins ich aber will sie pro Item (ist die y-variable)
    res1 <- reshape2::dcast(molten,
                            formula, fun, ..., margins = FALSE)
    formula_margins <-formula(
        paste("~", deparse(Formula[[2L]])))
    #- errechnet gesamt-Wert von y
    if (as.character(formula_margins)[2] == ".")
        data2 <- X$Y_data
    else
        data2 <- data

    margins2 <- Recast2(Formula = formula_margins, data = data2,
                        fun = fun, 
                        na.action = na.action, labels = labels,
                        margins = FALSE)

    ncl <- ncol(res1) - 2
    nrw <- nrow(margins2)
    margins2 <- cbind(
                  matrix(margins_name,
                    ncol = ncl, nrow = nrw,
                    dimnames = list(rownames(margins2),
                                names(res1)[1:ncl])),
                 margins2)

    result <- rbind(res1, margins2)
    if (labels & (!is.null(group_labels)))
          result <- set_label(result, group_labels)
  } else if (!default_formula) {
     if (is.null(X$X_data))
           data <- X$Y_data
     result <- reshape2::dcast(molten,
                              formula, fun,
                              margins = margins, ...)

    if (labels & (!is.null(group_labels)))
       result <- set_label(result, group_labels)
  } else {
    print("Recast2: Keine Ahnung wie ich hier her komme??")
    print("Eventuell: wegen margins? dann  fun = variable ~ ")
    result <- NULL
  }

  n <- length(result)
 if(ad_key_value) names(result)[c(n-1, n)] <- c(key, value)

  result
}


#' @rdname Recast2
#' @export
#' @description Summarise: ist eine Kopie bzw Erweiterung von Recast2
Summarise <- function(x,
                      .data,
                      fun = NULL,
                      key = "variable",
                      value = "value",
                    #  subset,
                      na.action = na.pass,
                      X = stp25formula::Formula_Data(x, .data, na.action= na.action),
                      id.vars = X$xname,
                      measure.var,
                      formula = NULL,
                      labels = TRUE,
                      margins = FALSE,
                      margins_name = "gesamt",
                      key.levels = NULL,
                      ...) {
funny <- list(...)
  
  if (length(funny) == 0) {
    res<-  Recast2(
      x,.data,fun, key, value,
      na.action,X,id.vars,
      measure.var,formula,
      labels, margins,
      margins_name
    )
  }
  else{
    res <-
      Recast2(
        x,  .data,  fun = funny[[1]],
        key,  value = names(funny)[1],  
        na.action,  X,  id.vars,
        measure.var,  formula = NULL,  labels,
        margins = FALSE,  margins_name
      )
    
    if (length(funny) > 1)
    {
      for (i in names(funny)[-1]) {
        next_res <- Recast2(
          x, .data,   fun = funny[[i]],
          key, value = i, 
          na.action, X, id.vars, measure.var, formula = NULL,
          labels, margins = FALSE,
          margins_name
        )
        res <-  cbind(res, next_res[ncol(next_res)])
      }
    }
  }
  
  if(!is.null(key.levels)){
    levels(res[,key]) <- key.levels
    
  }
  
  res
}