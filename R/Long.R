#' Long und Wide  
#'
#'  Erweiterung von tidyr::pivot_longer tidyr::pivot_wider
#' @param x data.frame oder formula
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
#' 
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
#' 
#' @examples 
#' 
#'   Long( .~ month, df2)
#'   
Long.formula <- function(x,
                         data,
                         key = "variable",
                         value = "value",
                         ...) {
  x <- stp25formula:::clean_dots_formula(x, names_data = names(data))
  rhs <- all.vars(x[-3])
  lhs <- all.vars(x[-2])
  
  data <- data[c(rhs, lhs)]
  lvl <-  get_label(data[rhs])
  
  rstl <-
    tidyr::pivot_longer(data,
                        cols = all_of(rhs),
                        names_to = key,
                        values_to = value)
  rstl[[key]] <- factor(rstl[[key]], names(lvl), lvl)
  
  rstl
}

#' @rdname Long
#' @export
Long.data.frame <- function(data,
                            ...,
                            by = NULL,
                            key = "variable",
                            value = "value",
                            id.vars = all.vars(by)) {
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
  
  if(length(measure.vars)==0){ 
    measure.vars  <- 
      if(length(id.vars)==0) names(data)  else names(data[-id.vars])
    }
  else {
    if(length(measure.vars)==1 & grepl('~', measure.vars[1] ))
      return( Long.formula(formula(measure.vars[1]), data,  key, value) )
       
    data <- data[c(measure.vars, id.vars)]
    
    }
  
  lvl <-  get_label(data[measure.vars])
  rstl <-
    tidyr::pivot_longer(data, 
                        cols = measure.vars,
                        names_to = key, values_to =value)
  rstl[[key]] <- factor(rstl[[key]], names(lvl), lvl)
  
  rstl
}


#' @param key.levels wenn value gesetzt wird dann 1:nlevels
#'
#' @rdname Long
#' @export
#' 
#' @examples 
#' 
#' Long(list( 
#'        A=c("Amy_A", "Bob_A"), 
#'        B=c( "Amy_B", "Bob_B")
#'        ),
#'      df2,
#'      by=~month,
#'      key = "student",
#'      key.level=c("Amy", "Bob")
#' )
#' 
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
