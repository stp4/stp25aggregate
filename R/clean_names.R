#' clean_names
#' Quelle https://drdoane.com/clean-consistent-column-names/
#'
#' @param .data data.frame oder character
#' @param tolower  alles zu kleinbuschstaben
#' @param unique eindeitige namen 
#' @param abbreviate,minlength  Abbkürzung
#'
#' @return die selbe Klasse wie dr Input
#' @export
#'
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
clean_names <-
  function(.data,
            tolower = FALSE,
           unique = TRUE,
           abbreviate = FALSE,
           minlength = 4) {
    n <- if (is.data.frame(.data))
      colnames(.data)
    else
      .data
    
    n <- gsub("%+", "_pct_", n)
    n <- gsub("\\$+", "_dollars_", n)
    n <- gsub("\\++", "_plus_", n)
    n <- gsub("-+", "_minus_", n)
    n <- gsub("\\*+", "_star_", n)
    n <- gsub("#+", "_cnt_", n)
    n <- gsub("&+", "_and_", n)
    n <- gsub("@+", "_at_", n)
    n <- gsub("\u00e4", "ae", n)
    n <- gsub("\u00fc", "ue", n)
    n <- gsub("\u00f6", "oe", n)
    n <- gsub("\u00dc", "Ue", n)
    n <- gsub("\u00c4", "Ae", n)
    n <- gsub("\u00d6", "Oe", n)
    n <- gsub("\u00df", "ss", n)
    n <- gsub("\u00A8", "", n)
    #   diaeresis <- "\u00A8" Sonderzeichen aus socisurvy
    n <- gsub("[^a-zA-Z0-9_]+", "_", n)
    n <- gsub("([A-Z][a-z])", "_\\1", n)
    n <- trimws(n)
    if (tolower)  n <- tolower(n)
    n <- gsub("(^_+|_+$)", "", n)
    
    if (unique)
      n <- make.unique(n, sep = "_")
    if (abbreviate) {
      n <- gsub("_+", "", n)
      n <- as.character( abbreviate(n, minlength))
    }
    else
      n <- gsub("_+", "_", n)
    if (is.data.frame(.data)) {
      colnames(.data) <- n
      .data
    } else {
      n
    }
  }

