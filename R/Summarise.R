#' Summarise Data
#' 
#' Summarise: ist eine  Erweiterung  aggregate()
#' 
#' @export
#' @param ... An Long() daten Formeln,Variablen-Namen
#' @param fun  function(x) length(na.omit(x)) an aggregate
#' @param key,value  an Long bzw
#' @param na.action  an aggregate
#' @param formula Zeilen/Spalten Lang/Weit
#' @param margins,margins_name  Gesamt
#'
#' @return reshape objek
#' @export
#' @examples
#' 
#'  mean3<- function(x) mean(x, round(na.rm=TRUE),2)
#' df <- data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#' 
#' Summarise(A + B ~ student, df )
#' 
#' df %>%  Summarise2(
#'   A,
#'   B,
#'   by =  ~ student+ month,
#'   fun = mean3,
#'   formula=student ~ month,
#'   margins = TRUE 
#' )
#' #Recast(chol0+chol1+chol6+chol12~g, hyper, mean, "Zeit", "Cholesterin")  
#' #Summarise(chol0+chol1+chol6+chol12~g, hyper, fun= mean,  key="Zeit", value="Cholesterin")
#' 
#' 
Summarise <- function(...,
                       fun = function(x)
                         length(na.omit(x)),
                       key = "variable",
                       value = "value",
                       na.action = na.pass,
                       formula = NULL,
                       margins = FALSE,
                       margins_name = "Total") {
  nmbr_msr <- 1
  molten <- Long(..., key = key, value = value)
  default_formula <-
    formula(paste(value, "~", paste(names(molten)[-ncol(molten)], collapse = "+")))
 
   rslts <-
    aggregate(default_formula,
              molten,
              FUN = fun,
              na.action = na.action)
  
  rslts <- rslts[order(rslts[[1]]), ]
  
  rst<- rslts[ncol(rslts)]
  if (class(rst[[1]])[1] == "matrix") {
    nmbr_msr <- colnames(rst[[1]])
    rslts <- cbind(rslts[-ncol(rslts)], rst[[1]])
  } else{
    names(rslts)[ncol(rslts)] <- value
  }
  
  if (isTRUE(margins)) {
    default_formula <-
      formula(paste(value, "~", 
                    paste(names(molten)[-((1:0) - ncol(molten))], 
                          collapse = "+")))
    rslts_m <-
      aggregate(default_formula,
                molten,
                FUN = fun,
                na.action = na.action)
    
     rst <- rslts_m[ncol(rslts_m)]
     if(class(rst[[1]])[1] == "matrix")
        rslts_m <- cbind(rslts_m[-ncol(rslts_m)], rst[[1]])
  
    rslts <- dplyr::bind_rows(rslts, rslts_m)
 
    frst<-  rslts[[1]]
    if(is.factor(frst))
        rslts[[1]] <-  factor(frst, c(levels(frst), margins_name))
    
    
    rslts[[1]][is.na(frst)] <- margins_name
  }
  
  if (!is.null(formula)) {
    rslts <-  Wide(formula, rslts, value)
  }
  rslts
}

#' @rdname Summarise
#' @export
Recast2<- function( Formula,
                    data,
                    fun = function(x) length(na.omit(x)),
                    key = "variable", value = "value",
                    na.action = na.pass,
                    X,
                    id.vars,  measure.var,
                    formula = NULL,
                    labels,
                    margins = FALSE,
                    margins_name = "gesamt"){
  Summarise(Formula,
             data,
             fun = fun,
             value = value,
             key = key,
             na.action = na.action,
             formula = formula,
             margins = margins,
             margins_name=margins_name)
}


## ## @rdname Summarise
## ##
## ## @description  Erweiterung der recast-Funktion. Erwartet einen data.frame
## ## mit \code{label} und verwendet die \code{melt} und \code{dcast}  Funftionen
## ## vom Packet library(reshape2).
## ## @param Formula   Formula-Objekt
## ## @param data   Daten
## ## @param fun Agregat Funktion
## ## @param key,value Names of new key and value columns, as strings
## ## @param na.action  fuer Formula na.action = na.omit, na.pass, na.exclude, na.fail
## ## @param formula  zum vertauchen von Spalten Argument an dcast
## ## @param labels sollen levels verwendet werden
## ## @param margins Gesamtwert bei TRUE eigene Funktion sonst dcast
## ## @param margins_name = "gesamt"  bei variablename weitergabe an dcast sonst droeel
## ## @param X,id.vars,measure.var Formula-Objekt,  Name der Gemessenen Variable
## ## @param ... recast Arguments
## ## @return reshape objek
## ## @export
## ## @examples
## ##
## ## #library(tidyverse)
## ## #library(stp25aggregate)
## ## #library(stp25data)
## ##
## ## hyper1<-hyper[, c("g","chol0","chol1","chol6","chol12")]
## ## hyper_long<- Melt2(hyper1, id.vars=1)
## ## aggregate( value~variable, hyper_long, mean)
## ##
## ## hyper_long<-Melt2(chol0+chol1+chol6+chol12~g,
## ##                   hyper, "Zeit", "Cholesterin")
## ## #-- Spread + aggragate
## ## aggregate(Cholesterin~Zeit+g, hyper_long, mean) %>%
## ##   spread(g, Cholesterin)
## ##
## ## #- das gleiche wie aggragate
## ## hyper_long %>% group_by(Zeit, g) %>%
## ##   summarise(Cholesterin=mean(Cholesterin)) %>%
## ##   spread(g, Cholesterin)
## ##
## ## #-- Gather das gleiche wie oben aber ohne die Labels
## ## hyper  %>%
## ##   tidyr::gather("time", "chol", chol0:chol12) %>%
## ##   dplyr::select(g, time, chol)
## ##
## ## #-- Recast2 das gleiche wie oben
## ## Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
## ##         "Zeit", "Cholesterin") %>%
## ##   spread(g, Cholesterin)
## ##
## ## #-- Recast2 das gleiche wie oben nur ohne  spread
## ## Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
## ##         formula=variable~g)
## ##
## ##
## ## Recast2(chol0+chol1+chol6+chol12~g, hyper,
## ##         mean,formula=variable~g, margins=TRUE)
## ##
## Recast2 <- function(Formula,
##                     data,
##                     fun = NULL,
##                     key = "variable",
##                     value = "value",
##                     
##                     na.action = na.pass,
##                     X = stp25formula::Formula_Data(Formula, data,
##                                                    na.action = na.action),
##                     id.vars = X$xname,
##                     measure.var,
##                     formula = NULL,
##                     labels = TRUE,
##                     margins = FALSE,
##                     margins_name = "gesamt",
##                     
##                     ...) {
##   default_formula <-
##     paste(paste(c(id.vars, "variable"), collapse = "+") , "~'value'")
##   mynames <- c(X$xname, X$yname)
##   
##   
##   if ("variable" %in% mynames | "value" %in% mynames)
##     warning(
##       "Die Variablen-Namen variable oder value sind nicht erlaubt in Recast2\nDie Namen waren: ",
##       paste(mynames, collapse = ", ")
##     )
##   
##   if (is.null(formula)) {
##     ad_key_value <- TRUE
##     formula <- default_formula
##     default_formula <- TRUE
##   } else{
##     ad_key_value <- FALSE
##     default_formula <- FALSE
##   }
##   
##   if (!all_identical2(X$Y_data)) {
##     print(head(X$Y_data))
##     cat("\nNicht berechenbar da falsche Skalenniveau (Faktor und Zahlen gemischt!)\n")
##     return(NULL)
##   }
##   data <- if (is.null(X$X_data))
##     X$Y_data
##   else
##     cbind(X$X_data, X$Y_data)
##   value_labels <- NULL
##   group_labels <- NULL
##   
##   molten <-
##     suppressWarnings(reshape2::melt(data, id.vars, measure.var))
##   
##   if (labels) {
##     value_labels <- get_label(X$Y_data)
##     group_labels <- get_label(X$X_data)
##     # --  wegen doppelter Labels
##     value_labels2 <-
##       if (length(unique(value_labels)) != length(value_labels))
##         paste0("(", 1:length(value_labels), ") ", value_labels)
##     else
##       value_labels
##     molten$variable <-
##       factor(molten$variable, names(value_labels), value_labels2)
##   }
##   if (default_formula & (isFALSE(margins))) {
##     result <-  reshape2::dcast(molten, formula, fun, ...)
##     if (labels & (!is.null(group_labels)))
##       result <-   set_label(result, labels = group_labels)
##     
##   } else if (default_formula &  isTRUE(margins)) {
##     #- Workaraound  fuer margins weil die nicht das gewuensche Ergebniss liefern
##     #- orginal dcast produzertz ueberall die margins ich aber will sie pro Item (ist die y-variable)
##     res1 <- reshape2::dcast(molten,
##                             formula, fun, ..., margins = FALSE)
##     formula_margins <- formula(paste("~", deparse(Formula[[2L]])))
##     #- errechnet gesamt-Wert von y
##     if (as.character(formula_margins)[2] == ".")
##       data2 <- X$Y_data
##     else
##       data2 <- data
##     
##     margins2 <- Recast2(
##       Formula = formula_margins,
##       data = data2,
##       fun = fun,
##       na.action = na.action,
##       labels = labels,
##       margins = FALSE
##     )
##     
##     ncl <- ncol(res1) - 2
##     nrw <- nrow(margins2)
##     margins2 <- cbind(matrix(
##       margins_name,
##       ncol = ncl,
##       nrow = nrw,
##       dimnames = list(rownames(margins2),
##                       names(res1)[1:ncl])
##     ),
##     margins2)
##     
##     result <- rbind(res1, margins2)
##     if (labels & (!is.null(group_labels)))
##       result <- set_label(result, group_labels)
##   } else if (!default_formula) {
##     if (is.null(X$X_data))
##       data <- X$Y_data
##     result <- reshape2::dcast(molten,
##                               formula, fun,
##                               margins = margins, ...)
##     
##     if (labels & (!is.null(group_labels)))
##       result <- set_label(result, group_labels)
##   } else {
##     print("Recast2: Keine Ahnung wie ich hier her komme??")
##     print("Eventuell: wegen margins? dann  fun = variable ~ ")
##     result <- NULL
##   }
##   
##   n <- length(result)
##   if (ad_key_value)
##     names(result)[c(n - 1, n)] <- c(key, value)
##   
##   result
## }



# Summarise <- function(x,
#                       data,
#                       fun = NULL,
#                       key = "variable",
#                       value = "value",
#                       na.action = na.pass,
#                       X = stp25formula::Formula_Data(x, data, na.action= na.action),
#                       id.vars = X$xname,
#                       measure.var,
#                       formula = NULL,
#                       labels = TRUE,
#                       margins = FALSE,
#                       margins_name = "gesamt",
#                       key.levels = NULL,
#                       ...) {
#   funny <- list(...)
#   
#   if (length(funny) == 0) {
#     res<-  Recast2(
#       x, data, fun, key, value,
#       na.action, X, id.vars,
#       measure.var, formula,
#       labels, margins,
#       margins_name
#     )
#   }
#   else{
#     res <-
#       Recast2(
#         x, data, fun = funny[[1]],
#         key, value = names(funny)[1],  
#         na.action, X,  id.vars,
#         measure.var,  formula = NULL,  labels,
#         margins = FALSE,  margins_name
#       )
#     
#     if (length(funny) > 1)
#     {
#       for (i in names(funny)[-1]) {
#         next_res <- Recast2(
#           x, data, 
#           fun = funny[[i]],
#           key, value = i, 
#           na.action, X, id.vars, measure.var, formula = NULL,
#           labels, margins = FALSE,
#           margins_name
#         )
#         res <- cbind(res, next_res[ncol(next_res)])
#       }
#     }
#   }
#   
#   if(!is.null(key.levels)){
#     levels(res[,key]) <- key.levels
#     }
#   
#   res
# }