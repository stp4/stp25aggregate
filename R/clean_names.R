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
#'
#' #' Alternative mit janitor#'
#'
#' cbind(x=x,
#' stp=stp25aggregate::clean_names(x),
#' jan=janitor::make_clean_names(x, replace=
#'                                 c(
#'                                   "'"="",
#'                                   "\""="",
#'                                   "%"="_pct",
#'                                   "#"="_cnt",
#'                                   "\u00e4"= "ae",
#'                                   "\u00fc"= "ue",
#'                                   "\u00f6"= "oe",
#'                                   "\u00dc"= "Ue",
#'                                   "\u00c4"= "Ae",
#'                                   "\u00d6"= "Oe",
#'                                   "\u00df"= "ss"
#'
#'                                 )
#' ))
#'
clean_names <- function(x, ...) {
  UseMethod("clean_names")
}



#' @rdname clean_names
#' @export
clean_names.data.frame <-
  function(.data,
           label = TRUE,
           labels = NULL,
          # ues.janitor=FALSE,
           ...) {
    nams_df <- names(.data)
    
    nams_clean <- make_clean.default(nams_df, ...)
    
    
    if (label) {
      if (is.null(labels)) {
        labels <- nams_df
      }
      else if (length(labels) != length(nams_clean)) {
        stop(" Laenge der labels muss gleich der laenge des DF sein!")
      }
      
      names(.data) <- nams_clean
      names(labels) <- nams_clean
      stp25aggregate:::label_data_frame(.data, labels)
    }
    else {
      names(.data) <- nams_clean
      .data
    }
    
  }


#' @rdname clean_names
#' @export
make_clean.default <-
  function(n,
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
               "_" ="."
             )) {
    n <- stringr::str_replace_all(str = n,
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



# make_clean_names <-
#   function(n,
#            tolower = FALSE,
#            unique = TRUE,
#            abbreviate = FALSE,
#            minlength = 4) {
#     n <- gsub("%+", "_pct_", n)
#     n <- gsub("\\$+", "_dollars_", n)
#     n <- gsub("\\++", "_plus_", n)
#     n <- gsub("-+", "_minus_", n)
#     n <- gsub("\\*+", "_star_", n)
#     n <- gsub("#+", "_cnt_", n)
#     n <- gsub("&+", "_and_", n)
#     n <- gsub("@+", "_at_", n)
#     n <- gsub("\u00e4", "ae", n)
#     n <- gsub("\u00fc", "ue", n)
#     n <- gsub("\u00f6", "oe", n)
#     n <- gsub("\u00dc", "Ue", n)
#     n <- gsub("\u00c4", "Ae", n)
#     n <- gsub("\u00d6", "Oe", n)
#     n <- gsub("\u00df", "ss", n)
#     n <- gsub("\u00A8", "", n)
#     
#     
#     
#     #   diaeresis <- "\u00A8" Sonderzeichen aus socisurvy
#     n <- gsub("[^a-zA-Z0-9_]+", "_", n)
#     n <- gsub("([A-Z][a-z])", "_\\1", n)
#     n <- trimws(n)
#     if (tolower)
#       n <- tolower(n)
#     n <- gsub("(^_+|_+$)", "", n)
#     
#     if (unique)
#       n <- make.unique(n, sep = "_")
#     if (abbreviate) {
#       n <- gsub("_+", "", n)
#       n <- as.character(abbreviate(n, minlength))
#     }
#     else
#       n <- gsub("_+", "_", n)
#     
#     n
#     
#   }



# 
# janitor_make_clean_names
# function (string,
#           case = "snake",
#           replace = c(
#             `'` = "",
#             `"` = "",
#             `%` = "_percent_",
#             `#` = "_number_"
#           ),
#           ascii = TRUE,
#           use_make_names = TRUE,
#           sep_in = "\\.",
#           transliterations = "Latin-ASCII",
#           parsing_option = 1,
#           numerals = "asis",
#           ...)
# {
#   if (case == "old_janitor") {
#     return(old_make_clean_names(string))
#   }
#   replaced_names <- stringr::str_replace_all(str = string,
#                                              pattern = replace)
#   transliterated_names <- if (ascii) {
#     stringi::stri_trans_general(replaced_names, id = available_transliterators(c(
#       "Any-Latin",
#       "Greek-Latin", "Latin-ASCII"
#     )))
#   }
#   else {
#     replaced_names
#   }
#   good_start <- stringr::str_replace(str = transliterated_names,
#                                      pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
#                                      replacement = "\\1")
#   made_names <- if (use_make_names) {
#     make.names(good_start)
#   }
#   else {
#     good_start
#   }
#   cased_names <- snakecase::to_any_case(
#     made_names,
#     case = case,
#     sep_in = sep_in,
#     transliterations = transliterations,
#     parsing_option = parsing_option,
#     numerals = numerals,
#     ...
#   )
#   while (any(duplicated(cased_names))) {
#     dupe_count <- vapply(seq_along(cased_names), function(i) {
#       sum(cased_names[i] == cased_names[1:i])
#     }, 1L)
#     cased_names[dupe_count > 1] <- paste(cased_names[dupe_count >
#                                                        1], dupe_count[dupe_count > 1], sep = "_")
#   }
#   cased_names
# }
