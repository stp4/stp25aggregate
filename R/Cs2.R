#' Character strings from unquoted names
#'
#' Erweiterung der  Hmisc Cs-Funktion um trim.ws. Erlaubt Variablen mit Komma oder Plus(+) oder Leerzeichen abzuternnen.
#' @param ... Vektor oder String oder Formula
#' @return Vektor
#' @export
#' @examples
#' Cs2(sd,fr,fg)
#' Cs2(sd,fr,fg, "hju nh")
#' Cs2("  Hallo Welt ")
#' Cs2("~ sd +    fr + fg")

Cs2 <-
  function (...)
  {
    x <- as.character(sys.call())[-1]
    if (length(x) == 1)
      strsplit(gsub("^[[:blank:]]*", "",
                    gsub(
                      "[[:blank:]]*$", "",
                      gsub("[\t\n\\~\\+\\:/]", "  ", x)
                    ))
               , " +")[[1]]
    else
      x
    
  }
# 
# 
# Cs2 <-
#   function (...)
#   {
#     # library(Hmisc)
#     trim_ws <-
#       function (text)
#         strsplit(gsub("^[[:blank:]]*", "",
#                       gsub(
#                         "[[:blank:]]*$", "",
#                         gsub("[\t\n\\~\\+\\:/]", "  ", text)
#                       ))
#                  , " +")[[1]]
# 
#     x <- as.character(sys.call())[-1]
#     if (length(x) == 1)
#       x <- trim_ws(x)
#     if (Hmisc::all.is.numeric(levels(factor(x))))
#       x <- as.numeric(as.character(x))
#     x
#   }
