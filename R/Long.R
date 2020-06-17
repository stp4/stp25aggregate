#' Umformen (reshape)
#'
#' Umformen von einem Breit-Format nach einem Lang-Format. Melt2 und melt2 sind
#' Erweiterungen der reshape2::melt Funktion. Intern wird  melt und dcast verwendet.
#' @param x Objekt kann Formel oder data.frame sein
#' @param ... weitere Argument 
#'
#' @return data.frame
#' @export
#' @examples 
#' 
#' df <- data.frame(month=rep(1:3,2),
#' student=rep(c("Amy", "Bob"), each=3),
#' A=c(9, 7, 6, 8, 6, 9),
#' B=c(6, 7, 8, 5, 6, 7))
#' 
#' df2<-df %>% Wide(student, c(A, B))
#' 
#' 
#' 
#' df[-4] %>% tidyr::spread(student, A)
#' df[-4] %>% Wide(student, A)
#' 
#' 
#' df2  %>% Long( Amy_A, Amy_B, Bob_A, Bob_B, by=~month)
#' 
#' 
#' df
#' Long(list( A=c("Amy_A", "Bob_A"), B=c( "Amy_B", "Bob_B")),
#'      df2,
#'      by=~month,
#'      key = "student",
#'      key.level=c("Amy", "Bob")
#' )
#' 
#' df %>%
#'   tidyr::gather(variable, value, -(month:student)) %>%
#'   tidyr::unite(temp, student, variable) %>%
#'   tidyr::spread(temp, value)
Long <- function(x, ...) {
  UseMethod("Long")
}

#' @rdname Long
#' @export
Long.formula <- function(x, data,  
                         key = "variable",
                         value = "value", 
                         ...) {
  is_tbl <- dplyr::is.tbl(data)
  molten <- Melt2.formula(x, data, 
                          key = key,
                          value = value,  
                          ...)
  if (is_tbl) tibble::as_tibble(molten)
  else molten
}

#' @rdname Long
#' @export
Long.data.frame <- function(data, ..., 
                            key = "variable",
                            value = "value",
                            id.vars = NULL) {
  is_tbl <- dplyr::is.tbl(data)
  if (is.null(id.vars))
    molten <- melt2(data, ..., key = key,
                    value = value)
  else
    molten <- Melt2.data.frame(data, id.vars=id.vars,  
                               key = key,
                               value = value, 
                               ...)
  
  if (is_tbl) tibble::as_tibble(molten)
  else molten
  
}


 
#' @param key.levels wenn value gesetzt wird dann 1:nlevels
#'
#' @rdname Long
#' @export
Long.list <- function(x,
                      data,
                      by = NULL,
                      key = NULL,
                      value = NULL,
                      key.levels = NULL,
                      ...) {
  if (!all(lengths(x)[1] == lengths(x)))
    stop("Die liste mus gleich lang sein!")
  is_tbl <- dplyr::is.tbl(data)
  
  first_var <- x[[1]]
  if (!is.null(by)) {
    if (is_formula2(by))
      by <- all.vars(by)
    dat <- data[c(by, first_var)]
    
  } else{
    dat <- data[first_var]
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
      dat <- data[next_var]
      next_molten <- melt2(dat, value = i,  ...)
      if (!is.null(key)) {
        next_molten <-  next_molten[-1]
      }
      
      molten <- cbind(molten, next_molten)
    }
  
  
  if (is_tbl) tibble::as_tibble(molten)
  else molten
}


 
# 
# Long <- function(x, ...) {
#   UseMethod("Long")
# }
# 
#  
# Long.formula <- function(x, data, ...) {
#   is_tbl <- dplyr::is.tbl(data)
#   molten <- Melt2.formula(x, data, ...)
#   if (is_tbl) tibble::as_tibble(molten)
#   else molten
# }
# 
#  
# Long.data.frame <- function(data, ..., id.vars = NULL) {
#   is_tbl <- dplyr::is.tbl(data)
#   if (is.null(id.vars))
#     molten <- melt2(data, ...)
#   else
#     molten <- Melt2.data.frame(data, id.vars=id.vars, ...)
# 
#   if (is_tbl) tibble::as_tibble(molten)
#   else molten
# 
# }



 
# Long.list <- function(x,
#                       data,
#                       by = NULL,
#                       key = NULL,
#                       value = NULL,
#                       key.levels = NULL,
#                       ...) {
#   if (!all(lengths(x)[1] == lengths(x)))
#     stop("Die liste mus gleich lang sein!")
#   is_tbl <- dplyr::is.tbl(data)
#   
#   first_var <- x[[1]]
#   if (!is.null(by)) {
#     if (is_formula2(by))
#       by <- all.vars(by)
#     dat <- data[c(by, first_var)]
#     
#   } else{
#     dat <- data[first_var]
#   }
#   molten <- melt2(dat, by = by, value = names(x)[1],  ...)
#   
#   if (!is.null(key)) {
#     if (is.null(key.levels))
#       key.levels <- 1:nlevels(molten$variable)
#     levels(molten$variable) <-  key.levels
#     names(molten)[(length(molten) - 1)] <- key
#   }
#   
#   if (length(x) > 1)
#     for (i in names(x)[-1]) {
#       next_var <- x[[i]]
#       dat <- data[next_var]
#       next_molten <- melt2(dat, value = i,  ...)
#       if (!is.null(key)) {
#         next_molten <-  next_molten[-1]
#       }
#       
#       molten <- cbind(molten, next_molten)
#     }
#   
#   
#   if (is_tbl) tibble::as_tibble(molten)
#   else molten
# }
 