#' @name CleanUp
#' @rdname CleanUp
#' @title CleanUp
#' @description Bereinigung der Labels
#' \code{CleanUp_factor()} Die Labels von tbl_df werden ja als labelled -Attribut abgelegt und durch cleanup werden sie f?r APA2 lesber clenaUp.tbl_df
#' @param data Date.Frame
#' @param ... extra arguments
#' @export

CleanUp <-
  function(data, ...) {
    UseMethod("CleanUp")
  }


#' @rdname CleanUp
#' @description Die \code{CleanUp.tbl_df()} bereinigt die Faktoren da die Auswertung
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
#' @param uselabels Labels
#' @param na.strings Fehlende Werte
#' @param force.numeric alles zu Zahlen
#' @param sep Trennzeichen
#' @param encoding Stichwort sonderzeichen
#' @param variable.labels Variable Labels
#' @param variable.names Variable Namen
CleanUp.default  <- function (data = NA,
                              uselabels = FALSE,
                              ###labels von Hmisc  FALSE
                              na.strings = NULL,
                              force.numeric = FALSE,
                              ####alles zu Zahlen konvertieren
                              encoding = TRUE,
                              #"UTF-8"
                              sep = ".",
                              #variable.labels   noch nicht getestet  attributes(data)$variable.labels,
                              variable.labels =  attributes(data)$variable.labels,
                              # Import von LimeSurvy
                              variable.names = names(data),
                              ...)
{
  mylabels <-
    if (is.null(variable.labels))
      Hmisc::label(data)
  else
    variable.labels
  
  
  
  mylabels <-
    gsub("[[:space:]]*$", "", mylabels) # alle Lehrzeichen am Ende
  if (uselabels) {
    mylabels <- gsub("[^[:alnum:]]", " ", mylabels)
    mylabels <- gsub("[ ]+", sep, mylabels)
    mylabels <-
      gsub(".$", "", mylabels) # alle Lehrzeichen am Ende
    # mylabels <-    gsub( ".\\s+","", mylabels ) # alle Lehrzeichen am Ende
  }
  mylabels <- ifelse(mylabels == "", names(mylabels), mylabels)
  
  cat("\n First 20 Labels:")
  print(mylabels[1:20])
  
  data <- data.frame(
    lapply(data, 
           function(x) {
             # 1  NA-remove
             if (!is.null(na.strings))
               x[which(x  %in%  na.strings)] <- NA
             
             if (any(class(x) == "factor")) {
               if (!force.numeric){
                 levels(x) <- Clean_String(levels(x), encoding)
                 factor(x,  levels(x))#sicherstellen dass nur Factor rauskommt
               }
               else{
                 as.numeric(x)  # 2 to numeric 
               }
             }
             else if(is.character(x)){  Clean_String(x, encoding)
               #if(!force.numeric)  x<-factor(x) else x<-as.numeric(factor(x)
             }
             else if(is.integer(x)) {as.numeric(x)}
             else if(is.atomic(x))  { as.numeric(x)}
             else {x}
           }
    )
  )
  if (uselabels)
    names(data) <- mylabels
  else {
    names(mylabels) <- names(data)
    #class(label(data))
    data <- label_data_frame(data, labels = mylabels)
    
  }
  
  data 
}
# CleanUp.default  <- function (data = NA,
#                               uselabels = FALSE,
#                               ###labels von Hmisc  FALSE
#                               na.strings = NULL,
#                               force.numeric = FALSE,
#                               ####alles zu Zahlen konvertieren
#                               encoding = NULL,
#                               #"UTF-8"
#                               sep = ".",
#                               #variable.labels   noch nicht getestet  attributes(data)$variable.labels,
#                               variable.labels =  attributes(data)$variable.labels,
#                               # Import von LimeSurvy
#                               variable.names = names(data),
#                               ...)
# {
#   iconv.data.frame <-
#     function(df, from = "UTF8", to = "latin1", ...) {
#       df.names <- iconv(names(df), from , to)
#       df.rownames <- iconv(rownames(df), from , to)
#       df.label <- iconv(Hmisc::label(df),  from , to)
# 
#       names(df) <- df.names
#       rownames(df) <- df.rownames
#       df.list <- lapply(df, function(x) {
#         if (any(class(x) == "factor")) {
#           levels(x)  <- iconv(levels(x), from , to)
#           x
#         } else if (any(class(x) == "character")) {
#           x <- iconv(x, from , to)
#         } else{
#           x
#         }
#       })
#       names(df.label) <- df.names
# 
#       upData2(data.frame(df.list), labels = df.label)
#     }
# 
# 
#   mylabels <-
#     variable.labels <-
#     if (is.null(variable.labels))
#       Hmisc::label(data)
#   else
#     variable.labels
# 
#   mylabels <-
#     gsub("[[:space:]]*$", "", mylabels) # alle Lehrzeichen am Ende
#   if (uselabels) {
#     mylabels <- gsub("[^[:alnum:]]", " ", mylabels)
#     mylabels <- gsub("[ ]+", sep, mylabels)
#     mylabels <-
#       gsub(".$", "", mylabels) # alle Lehrzeichen am Ende
#     # mylabels <-    gsub( ".\\s+","", mylabels ) # alle Lehrzeichen am Ende
#   }
#   mylabels <- ifelse(mylabels == "", names(mylabels), mylabels)
# 
#   data <- data.frame(lapply(data, function(x) {
#     if (!is.null(na.strings))
#       x[which(x  %in%  na.strings)] <- NA
#     if (any(class(x) == "factor")) {
#       if (!force.numeric)
#         x <- factor(x)
#       else
#         x <- as.numeric(x)
#     }
#     # else if(any(class(x)=="character")){  if(!force.numeric)  x<-factor(x) else x<-as.numeric(factor(x)  }
#     else
#       x <- as.numeric(x)
#     return(x)
#   }))
#   if (uselabels)
#     names(data) <- mylabels
#   else {
#     names(mylabels) <- names(data)
#     #class(label(data))
#     data <- label_data_frame(data, labels = mylabels)
# 
#   }
# 
# 
#   if (!is.null(encoding))
#     data <- iconv.data.frame(data)
#   return(data)
# }
# 




#' @rdname CleanUp
#' @description CleanUp_factor bereinigen von haven Label
#' @export
CleanUp_factor <-
  function(data, ...) {
    #09-01-2019 aenderung in haven haven_labelled
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
    if (inherits(data, "tbl_df"))
      data
    else
      dplyr::tbl_df(data)
  }



# CleanUp_factor <- function(data, ...) {
#   y <- sapply(data, function(x)
#     inherits(x, "labelled"))
#   if (sum(y) > 0)
#     data[y] <- lapply(data[y], Convert_To_Factor)
#   if (inherits(data, "tbl_df"))
#     data
#   else
#     dplyr::tbl_df(data)
# }




cleanup_typ_character_encoding <-
  function(data, from = "UTF8" , to = "latin1") {
    myFact <-
      which(sapply(data, function(x)
        inherits(x, "character")) == TRUE)
    if (length(myFact) > 0) {
      for (i in myFact)
        data[, i] <-  iconv(data[, i], from , to)
    }
    data
  }
cleanup_factor_levels_encoding <-
  function(data, from = "UTF8" , to = "latin1") {
    myFact <-
      which(sapply(data, function(x)
        inherits(x, "factor")) == TRUE)
    if (length(myFact) > 0) {
      for (i in myFact)
        levels(data[, i]) <-  iconv(levels(data[, i]), from , to)
    }
    data
  }
#interne funktion fuer factor und cleanup
Convert_To_Factor <- function(x) {
  # --labels lassen sich nicht direkt auslesen

  lbl <- if (any(names(attributes(x)) == "label"))  attr(x, "label")  else  names(x)
  
  lbls <- attr(x, "labels")

  #SPSS erlaubt leere Labels daher diese auffuellen
  
  if( any(names(lbls)=="") | any(names(lbls)=="&nbsp;") ){
    spss_names <- gsub("&nbsp;", "", names(lbls))
    
    empty <- which(spss_names=="")
    names(lbls)[empty] <- empty
  }

  x <- factor(x, lbls, names(lbls))
  # x <- haven::as_factor(x) Labels werden Falsch geordnet wenn zB
  # 1=ja, 8=nein dabei wird 8 verworfen

  attr(x, "label") <- lbl
  x
}
