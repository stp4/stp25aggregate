#' Summarise Data
#' 
#' Summarise: ist eine  Erweiterung  aggregate()
#' 
#' @export
#' @param ... An Long 
#' @param fun  function(x) length(na.omit(x)) an aggregate
#' @param key,value  
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
  molten <- suppressWarnings(Long(..., 
                                  key = key, 
                                  value = value))

  default_formula <-
    formula(paste(value, "~", paste(names(molten)[-ncol(molten)], collapse =
                                      "+")))
  rslts <-
    aggregate(default_formula,
              molten,
              FUN = fun,
              na.action = na.action)
  rslts <- rslts[order(rslts[[1]]), ]
  
  rst<- rslts[ncol(rslts)]
  if(class(rst[[1]])[1] =="matrix"){
    nmbr_msr<- colnames(rst[[1]])
    rslts <- cbind(rslts[-ncol(rslts)], rst[[1]])
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
        rslts[[1]] <-  factor(frst, c(levels( frst), margins_name))
    rslts[[1]][is.na(frst)] <- margins_name
  }
  
  if (!is.null(formula)) {
    rslts <- if (is.character(formula)) Wide(rslts,  key, value) 
             else reshape2::dcast(rslts, formula , value.var = value)
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
                    X ,
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