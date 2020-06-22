#' @name CleanUp
#' @rdname CleanUp
#' @title CleanUp
#' @description Bereinigung der Labels
#' \code{CleanUp_factor()} Die Labels von tibble::tibble werden ja als labelled -Attribut abgelegt und durch cleanup werden sie f?r APA2 lesber clenaUp.tibble::tibble
#' @param data Date.Frame
#' @param ... extra arguments
#' @export

CleanUp <-
  function(data, ...) {
    UseMethod("CleanUp")
  }


#' @rdname CleanUp
#' @description Die \code{CleanUp.tibble::tibble()} bereinigt die Faktoren da die Auswertung
#' direkt oft nicht funktioniert
#' @export
CleanUp.tbl_df <- function(data, ...)
  CleanUp_factor(data, ...)


#' @rdname CleanUp
#' @export
#' @param from encoding from UTF8
#' @param to encoding to latine1
cleanup_names_encoding <-
  function(data, from = "UTF8" , to = "latin1") {
    #-    ?enc2utf8
    # gsub("\\$", "", "$Hallo")
    names(data) <- gsub("\\$", "", iconv(names(data), from, to))
    data
  }




Clean_String <-
  function(x,
           encoding = NULL,
           #  from = "UTF8",
           to = "latin1") {
    if (!is.null(encoding))
      if (is.logical(encoding))
        x <-  rvest::repair_encoding(x)
      else
        x <- iconv(x, encoding , to)
      x <- stringr::str_trim(x)
      x <- stringr::str_replace_all(x, "\\p{quotation mark}", "'")
      x <- gsub("[\\\n\\\t\\\r]", " ", x)
      x <-   gsub(" +", " ", x)
      
      
      x
  }

#' @rdname CleanUp
#' @export
#' @param uselabels Labels labels   FALSE
#' @param na.strings Fehlende Werte
#' @param force.numeric alles zu Zahlen alles zu Zahlen konvertieren
#' @param sep Trennzeichen variable.labels   noch nicht getestet  attributes(data)$variable.labels,
#' @param encoding Stichwort sonderzeichen  "UTF-8"
#' @param variable.labels Variable Labels  Import von LimeSurvy
#' @param variable.names Variable Namen
CleanUp.default  <- function (data = NA,
                              uselabels = FALSE,
                              na.strings = NULL,
                              force.numeric = FALSE,
                              encoding = TRUE,
                              sep = ".",
                              variable.labels =  attributes(data)$variable.labels,
                              variable.names = names(data),
                              ...)
{
  
  cat("\nCleanUp: \n", 
      "\n uselabels =",uselabels,
      "\n na.strings = ",na.strings,
      "\n force.numeric =" ,force.numeric,
      "\n encoding = ",encoding,
      "\n sep = ", sep, "\n"
      )
  
  mylabels <-
    if (is.null(variable.labels)) get_label(data)  else variable.labels
  
  # alle Lehrzeichen am Ende
  mylabels <-  gsub("[[:space:]]*$", "", mylabels)
  if (uselabels) {
    mylabels <- gsub("[^[:alnum:]]", " ", mylabels)
    mylabels <- gsub("[ ]+", sep, mylabels)
    mylabels <- gsub(".$", "", mylabels)
  }
  mylabels <- ifelse(mylabels == "", names(mylabels), mylabels)
  
  # cat("\n First 20 Labels:")
  # print(mylabels[1:20])
  
  data <- data.frame(lapply(data,
                            function(x) {
                              if (!is.null(na.strings)){
                              
                                x[which(x  %in%  na.strings)] <- NA
                                }
                              
                              if (any(class(x) == "factor")) {
                                if (!force.numeric) {
                                 # cat("\n", names(x), "Clean_String")
                                  levels(x) <- Clean_String(levels(x), encoding)
                                  #sicherstellen dass nur Factor rauskommt
                                  factor(x,  levels(x))
                                }
                                else{
                                  # 2 to numeric
                                  as.numeric(x)
                                }
                              }
                              else if (is.character(x)) {
                                Clean_String(x, encoding)
                              }
                              else if (is.integer(x)) {
                                as.numeric(x)
                              }
                              else if (is.atomic(x))  {
                                as.numeric(x)
                              }
                              else {
                                x
                              }
                            }))
  if (uselabels)
    names(data) <- mylabels
  else {
    names(mylabels) <- names(data)
    
    data <- set_label(data, labels = mylabels)
  }
  
  data
}



#' @rdname CleanUp
#' @description CleanUp_factor bereinigen von haven Label
#' @export
CleanUp_factor <- function(data,
                           ...) {
  y <- sapply(data, function(x) {
    if (inherits(x, "haven_labelled"))
      TRUE
    else if (inherits(x, "labelled"))
      TRUE
    else
      FALSE
  })
  
  if (sum(y) > 0)
    data[which(y)] <-
      lapply(data[which(y)], Convert_To_Factor)
  if (tibble::is_tibble(data))
    data
  else
    tibble::as_tibble(data)
}



#' @noRd
#'
#'
cleanup_typ_character_encoding <- function(data,
                                           from = "UTF8",
                                           to = "latin1") {
  myFact <-
    which(sapply(data, function(x)
      inherits(x, "character")) == TRUE)
  if (length(myFact) > 0) {
    for (i in myFact)
      data[, i] <-  iconv(data[, i], from , to)
  }
  data
}

#' @noRd
#'
#'

cleanup_factor_levels_encoding <- function(data,
                                           from = "UTF8",
                                           to = "latin1") {
  myFact <-
    which(sapply(data, function(x)
      inherits(x, "factor")) == TRUE)
  if (length(myFact) > 0) {
    for (i in myFact)
      levels(data[, i]) <-  iconv(levels(data[, i]), from , to)
  }
  data
}





#' Convert_To_Factor
#' 
#' interne funktion fuer factor und cleanup
#' labels lassen sich nicht direkt auslesen
#'
#' SPSS erlaubt leere Labels daher diese auffuellen
#'
#' x <- haven::as_factor(x) Labels werden Falsch geordnet wenn zB
#'
#'  1=ja, 8=nein dabei wird 8 verworfen
#' @noRd
#' @param x Objekt
Convert_To_Factor <- function(x) {
  lbl <-
    if (any(names(attributes(x)) == "label"))
      attr(x, "label")
  else
    names(x)
  
  lbls <- attr(x, "labels")
  
  if (any(names(lbls) == "") | any(names(lbls) == "&nbsp;")) {
    spss_names <- gsub("&nbsp;", "", names(lbls))
    empty <- which(spss_names == "")
    names(lbls)[empty] <- empty
  }
  x <- factor(x, lbls, names(lbls))
  attr(x, "label") <- lbl
  x
}
