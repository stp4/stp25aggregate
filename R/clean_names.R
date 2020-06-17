#' clean_names
#' 
#' 
#' Quelle https://drdoane.com/clean-consistent-column-names/
#'
#' @param data data.frame oder character
#' @param tolower  alles zu kleinbuschstaben
#' @param unique eindeitige namen
#' @param abbreviate,minlength  Abbkürzung
#'
#' @return die selbe Klasse wie dr Input
#' @export
#' @examples
#'
#'
#' clean_names(tibble::tibble("Öli"=1:3, "p-k"=1:3, "95%-CI"=4:6) )
#'
#' clean_names(
#'   c(
#'     "  a", "a  ", "a %", "a",
#'     "$a", "$$$a", "GDP ($)",
#'     "GDP (us$)", "a (#)", "a & b",
#'     "#", "$", "a_cnt", "Aa&Bb",
#'     "camelCasePhrases", "AlphaBetaGamma",
#'     "Alpha       Beta", "Beta  !!! Gamma",
#'     "a + b", "a - b", "a * b", "Ösel"
#'   ), abbreviate=TRUE
#' )
#'
#' #' Alternative mit janitor#'
clean_names <- function(x, ...) {
  UseMethod("clean_names")
}


#' @rdname clean_names
#' @description \code{clean_names} Input data.frame  output ist ein 
#' data.frame mit bereinigten namen.
#' @export
clean_names.data.frame <-
  function(data,
           label = TRUE,
           labels = NULL,
           # ues.janitor=FALSE,
           ...) {
    nams_df <- names(data)
    
    nams_clean <- clean_names.default(nams_df, ...)
    
    
    if (label) {
      if (is.null(labels)) {
        labels <- nams_df
      }
      else if (length(labels) != length(nams_clean)) {
        stop(" Laenge der labels muss gleich der laenge des DF sein!")
      }
      
      names(data) <- nams_clean
      names(labels) <- nams_clean
      stp25aggregate:::label_data_frame(data, labels)
    }
    else {
      names(data) <- nams_clean
      data
    }
    
  }


#' @param x 
#'
#' @param tolower 
#' @param unique 
#' @param abbreviate 
#' @param minlength 
#' @param replace 
#'
#' @rdname clean_names
#' @export
clean_names.default <-
  function(x,
           tolower = TRUE,
           unique = TRUE,
           abbreviate = FALSE,
           minlength = 4,
           replace =
             c(
               "'" = "",
               "\"" =  "",
               "%" =  "_pct",
               "#" =  "_cnt",
               "\u00e4" = "ae",
               "\u00fc" = "ue",
               "\u00f6" = "oe",
               "\u00dc" = "Ue",
               "\u00c4" = "Ae",
               "\u00d6" = "Oe",
               "\u00df" = "ss",
               #   diaeresis <- "\u00A8" Sonderzeichen aus socisurvy
               "\u00A8" = "",
               # "\\++" = "_plus_",
               # "-+" = "_minus_",
               # "\\*+" = "_star_",
               
               "&+" = "_and_",
               "@+" = "_at_",
               "_" = "."
             )) {
    n <- stringr::str_replace_all(str = x,
                                  pattern = replace)
    n <- trimws(n)
    n <- gsub("[^a-zA-Z0-9_]+", "\\.", n)
    #n <- gsub("([A-Z][a-z])", "_\\1", n)
    #
    n <- gsub("(^\\.+|\\.+$)", "", n)
    n <- make.names(n)
    
    if (tolower)
      n <- tolower(n)
    
    if (abbreviate) {
      n <- gsub("\\.+", "", n)
      n <- abbreviate(n, minlength, named = FALSE)
    }
    
    if (unique)
      n <- make.unique(n, sep = ".")
    
    n
  }
