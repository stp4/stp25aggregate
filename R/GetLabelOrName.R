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
  }else {
    xnames  <- names(x)
    # xlabel <- if( Hmisc_label) Hmisc::label(x) else xnames
    xlabel <- Hmisc::label(x)
    df_names <-  if (is.null(pattern))
      xnames
    else
      gsub(" $","",
           gsub(pattern, replacement, xnames), perl = TRUE)
    #   gsub(pattern, replacement, names(x), perl=T )
    xlabel <-
      ifelse(xlabel == "", df_names, xlabel)

    names(xlabel) <- xnames
    return(xlabel)
  }
}
