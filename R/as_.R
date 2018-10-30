#' @name as_irgenwas2
#' @rdname as_irgenwas2
#' @title as_Familie
#' @description Prueft ob objekt bestimmte Eigenschaften aufweist.
#' Fuer Dataframe gibt es \code{is_all_identical2()}
#' @param x zu pruefendes Objekt
#' @return Output ist ein Vector bzq dataframe mit gleicgher Laenge wie der Input.
#' @examples
#' data<- data.frame(a=1:10,
#'   b=as.character(1:10),
#' c=gl(2, 5, labels = c("Control", "Treat")),
#' d=gl(2, 5, labels = c("1", "3"))
#'
#' )
#' as_numeric(data$a)+1
#' as_numeric(data$b)+1
#' as_numeric(data$c)+1
#' as_numeric(data$d)+1
#' as_numeric(data)
#'
#'
#' #'
#' x <- c("0", "-3",  " 5", " 5.6", "   3,5  ", "super")
#' as_numeric(factor(x))
#' as_numeric(x)
#' x <- c("0", "-3",  " 5", " 5.6", "   3,5  ")
#'
#' as_numeric(factor(x))
#' as_numeric(x)
#'
#'
#' as_numeric(c("20:00:23", "56:14:47", "99:99:99", "00:00:00"), na.strings =
#'              "99:99:99")
#'
NULL
#' @rdname as_irgenwas2
#' @description  as_numeric fuer factor und string-objekte
#' @param na.strings Fehlende Werte
#' @param type factor, numeric, time
#' @export
#' @examples
#' x<- c("0", "-3",  " 5", " 5.6", "   3,5  ", "super")
#' ##character_to_numeric(factor(x))
#'
#' x<- c("0", "-3",  " 5", " 5.6", "   3,5  ")
#' ##character_to_numeric(x)
#'

#' @importFrom stringr str_count
#' @importFrom lubridate hms
as_numeric <- function(x,
                       na.strings = NULL,
                       type = guess_type(x)) {
  # print(type)
  #-- library(Hmisc )
  if (type == "factor") {
    levels(x) <- clean_space(levels(x))
    character_to_numeric(x, na.strings)
  } else if (type == "character") {
    character_to_numeric(as.factor(clean_space(x)), na.strings)
  }
  else if (type == "numeric")
    as_num(x, na.strings)
  else if (type == "time")
    as_time(x, na.strings)
  else if (is.data.frame(x))
    dapply2(x, function(x)
      character_to_numeric(x, na.strings = na.strings))
  else
    rep(NA,  length.out = length(x))
}

guess_type <- function(x) {
  if (is.numeric(x) | is.integer(x))
    "numeric"
  else  if (is.factor(x)) {
    lvl <- levels(x)
    if (any(stringr::str_count(lvl, "\\:") > 1))
      "time"
    else
      "factor"
    
  }
  else  if (is.character(x))  {
    if (any(stringr::str_count(x, "\\:") > 1))
      "time"
    else
      "character"
    
  }
  else
    "unknown"
}




character_to_numeric <-
  function (x, na.strings = NULL)
  {
    if (!is.null(na.strings))
      x[x == na.strings] <- NA
    
    nx <- nlevels(x)
    lx <- levels(x)
    lx <- gsub("[^0-9.-]+", NA, lx)
    lx <- unique(lx)
    
    if (nx == length(lx))
      as.numeric(as.character(x))
    else
      as.numeric(x)
  }


as_num <- function(x, na.strings = NULL) {
  if (!is.null(na.strings))
    x[x == na.strings] <- NA
  as.numeric(as.character(x))
}

as_time <- function(x, na.strings = NULL) {
  if (!is.null(na.strings))
    x[x == na.strings] <- NA
  as.numeric(lubridate::hms(x))
  
  
}
