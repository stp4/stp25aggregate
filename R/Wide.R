#' @rdname Long
#' @description 
#' Transformiert von Long nach Wide
#' Quelle: https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
#'
#' WArnung: die Funktion ist experimental
#' @export
#'
#' @examples
#' #
#'   suppressPackageStartupMessages(library(tidyverse))
#'
#' dat <- data.frame(
#'   month = rep(1:3, 2),
#'   student = factor(rep(c("Amy", "Bob"), each = 3)),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#' 
#' dat %>% Wide(student,  c(A, B))
#' dat %>% Wide(student,  c("A", "B"))
#' dat[-3] %>% Wide(student,  B)
#' dat  %>% Wide(student ~ month)
#' #dat[-3] %>% reshape2::dcast(month ~ student)
#' dat  %>% Wide(month ~ student, A)
#' dat  %>% Wide(student ~ month, A)
#' 
Wide <- function(data, key, value) {
  # if (!dplyr::is.tbl(data))
  #  data <- tibble::as_tibble(data)
  
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  
  # Formula Interface -------------------------------------------------------
  
  
  value_names <-
    paste(rlang::quo_get_expr(valueq))
  key_names <-  rlang::quo_get_expr(keyq)
  
  if (rlang::is_formula(key_names)) {
    lhs <- all.vars(key_names[[3]])
    
    if (value_names[1] == "")
      value_names <- guess_value(data)
    
    if (length(value_names) == 1) {
      data <- data[c(all.vars(key_names), value_names)]
      return(tidyr::spread(data, !!lhs, !!value_names))
    } else{
      # value_names <- value_names[-1]
      # data <- data[c(all.vars(key_names), value_names)]
      stop("Das geht nicht! Alternative ist Wide(key,  c(A, B))")
    }
  }
  
  # spread ------------------------------------------------------------------
  
  # test length value
  if (length(rlang::quo_get_expr(valueq)) == 1) {
    return(tidyr::spread(data, !!keyq, !!valueq))
  }
  # multi-value -------------------------------------------------------------
  
  
  s <- rlang::quos(!!valueq)
  dat_unite <-
    tidyr::unite(tidyr::gather(data, variable, value, !!!s),
                 temp,!!keyq,
                 variable,
                 sep = "_")
  dat_unite$temp <- factor(dat_unite$temp,
                           paste0(
                             stringr::str_split(
                               levels(factor(dat_unite$temp)), "_" , simplify = T)[, 1],
                             "_",
                             value_names[-1]
                           ))
  return(tidyr::spread(dat_unite, temp, value))
}
#reshape2:::guess_value
guess_value <- function (df)
{
  if ("value" %in% names(df))
    return("value")
  
  last <- names(df)[ncol(df)]
  message("Using ", last, " as value column: use value to override.")
  last
}


# Error dat  %>% Wide(month ~ student, c(A, B))
# Wide <- function(data, key, value) {
#   # if (!dplyr::is.tbl(data))
#   #  data <- tibble::as_tibble(data)
#   
#   # quote key
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   
#   # test length value
#   if (length(rlang::quo_get_expr(valueq)) == 1) {
#     tidyr::spread(data, !!keyq, !!valueq)
#   } else{
#     s <- rlang::quos(!!valueq)
#     value_names <-
#       paste(rlang::quo_get_expr(valueq))[-1]
#     
#     dat_unite <- 
#       tidyr::unite(
#             tidyr::gather(data, variable, value, !!!s),
#             temp,!!keyq, variable, sep = "_")
#     
#     dat_unite$temp <- factor(
#                        dat_unite$temp,
#                         paste0(stringr::str_split(
#                           levels(factor(dat_unite$temp)), "_", simplify = T)[, 1],
#                           "_", value_names))
#     
#     tidyr::spread(dat_unite, temp, value)
#   }
# }




# Wide <- function(data, key, value) {
#  # if (!dplyr::is.tbl(data))
#   #  data <- tibble::as_tibble(data)
#   # quote key
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   
#   # test length value
#   if (length(rlang::quo_get_expr(valueq)) == 1) {
#     tidyr::spread(data, !!keyq, !!valueq)
#   } else{
#     s <- rlang::quos(!!valueq)
#     
#     tidyr::spread(
#       tidyr::unite(
#         tidyr::gather(data, variable, value, !!!s),
#       temp, !!keyq, variable),
#     temp, value
#     )
#   }
# }


# Wide <- function(data, key, value) {
#   
#   # if (!dplyr::is.tbl(data))
#   #  data <- tibble::as_tibble(data)
#   
#   
#   
#   # quote key
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   
#   # test length value
#   if (length(rlang::quo_get_expr(valueq)) == 1) {
#     tidyr::spread(data, !!keyq, !!valueq)
#   } else{
#     s <- rlang::quos(!!valueq)
#     
#     tidyr::spread(
#       tidyr::unite(
#         tidyr::gather(data, variable, value, !!!s),
#         temp, !!keyq, variable),
#       temp, value
#     )
#   }
# }