#' as_numeric
#' 
#' String und Factor zu Numeric.
#' 
#' @param x vector
#' @param na.strings Fehlende Werte
#'
#' @param type factor, numeric, time
#' 
#' @export
#' 
#' @examples 
#' 
#' x <- c("0","-3","+5"," 5.6","   3,5  ","3,,4","3-5","uk","na",-77,-99,-999)
#' f<- factor(c(4,  16, 8,  1,  4,  1,  2,  4,  2,  2,  -77,  4 ))
#' 
#' data.frame(x=x,
#'            x_as_num=as_numeric(x, na.strings = c("-77", "-99")),
#'            
#'            f=f,
#'            f_as_num=as_numeric(f, na.strings = c("-77", "-99"))) 
as_numeric <- function(x,
                       na.strings = NULL,
                       type = class(x)) {
  if (is.character(x))
    character_to_numeric(x, na.strings)
  else  if (is.factor(x)) {
    if (type == "factor") {
      factor_to_numeric(x, na.strings)
    }
    else
      character_to_numeric(as.character(x), na.strings)
  }
  else
    x
  
}

factor_to_numeric <-   function (x, na.strings = NULL)
{
  if (!is.null(na.strings))
    x[x %in% na.strings] <- NA
  as.numeric(x)
  
}

character_to_numeric <-
  function (x, na.strings = NULL)
  {
    if (!is.null(na.strings))
      x[x %in% na.strings] <- NA
    x <- gsub(",+", ".", x)
    as.numeric(gsub("[^0-9.-]+", "", x))
  }



clean_space <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  sub(",", ".", x)
}
