#' GetLabelOrName Hmisc labels
#'
#' @description  Extrahiert die Labels
#' @param x  Data.frame
#' @param pattern pattern = "[\\._]+"
#' @param replacement Leerzeichen
#' @author Wolfgang Peter
#' @export
#' @examples
#' # GetLabelOrName(data, pattern="störender string")
#'
GetLabelOrName <- function(x,
                           pattern = NULL,
                           replacement = " ") {
  if (length(x) < 1) {
    return(NULL)
  } else {
    xnames  <- names(x)
    # xlabel <- if( Hmisc_label) Hmisc::label(x) else xnames
    xlabel <-  get_label(x) # Hmisc::label(x)
    df_names <- if (is.null(pattern)) xnames
                else gsub(" $", "", gsub(pattern, replacement, xnames), 
                          perl = TRUE)
    #   gsub(pattern, replacement, names(x), perl=T )
   # xlabel <- ifelse(xlabel == "", df_names, xlabel)
    
    is_units <- sapply(x, function(z) inherits(z, "units"))
    if (any(is_units)) {
      my_units <-
        sapply(x, function(z)
          if (inherits(z, "units"))
            paste0(" [", as.character(attr(z, "units")), "]")
          else
            "")
      xlabel <-  paste0(xlabel, my_units)
      names(xlabel) <- xnames
    }
  }
  xlabel
}


get_label <- function(x) {
  lbl <- lapply(x, attr, "label")
  unlabl <- which(sapply(lbl, is.null))
  lbl[unlabl] <- names(lbl[unlabl])
  unlist(lbl)
  
}