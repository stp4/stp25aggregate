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
                              encoding = NULL,
                              #"UTF-8"
                              sep = ".",
                              #variable.labels   noch nicht getestet  attributes(data)$variable.labels,
                              variable.labels =  attributes(data)$variable.labels,
                              # Import von LimeSurvy
                              variable.names = names(data),
                              ...)
{
  iconv.data.frame <-
    function(df, from = "UTF8", to = "latin1", ...) {
      df.names <- iconv(names(df), from , to)
      df.rownames <- iconv(rownames(df), from , to)
      df.label <- iconv(Hmisc::label(df),  from , to)

      names(df) <- df.names
      rownames(df) <- df.rownames
      df.list <- lapply(df, function(x) {
        if (any(class(x) == "factor")) {
          levels(x)  <- iconv(levels(x), from , to)
          x
        } else if (any(class(x) == "character")) {
          x <- iconv(x, from , to)
        } else{
          x
        }
      })
      names(df.label) <- df.names

      upData2(data.frame(df.list), labels = df.label)
    }


  mylabels <-
    variable.labels <-
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

  data <- data.frame(lapply(data, function(x) {
    if (!is.null(na.strings))
      x[which(x  %in%  na.strings)] <- NA
    if (any(class(x) == "factor")) {
      if (!force.numeric)
        x <- factor(x)
      else
        x <- as.numeric(x)
    }
    # else if(any(class(x)=="character")){  if(!force.numeric)  x<-factor(x) else x<-as.numeric(factor(x)  }
    else
      x <- as.numeric(x)
    return(x)
  }))
  if (uselabels)
    names(data) <- mylabels
  else {
    names(mylabels) <- names(data)
    #class(label(data))
    data <- label_data_frame(data, labels = mylabels)

  }


  if (!is.null(encoding))
    data <- iconv.data.frame(data)
  return(data)
}





#' @rdname CleanUp
#' @export
CleanUp_factor <- function(data, ...) {
  y <- sapply(data, function(x)
    inherits(x, "labelled"))
  if (sum(y) > 0)
    data[y] <- lapply(data[y], Convert_To_Factor)
  if (inherits(data, "tbl_df"))
    data
  else
    dplyr::tbl_df(data)
}




#' @rdname CleanUp
#' @description Die Funktion \code{Label()} erstellt die
#' Labels die bei den Tabellen
#' verwendet werden. Im Unterschied zu \code{UpData2()} arbeitet diese Funktion mit
#'   (...) argument.
#' @export
#' @examples
#'
#' #-- Label
#' # Steko<- GetData("Steko.sav") %>%
#' # Drop_NA(key) %>%
#' #   mutate(jahr = factor(jahr)) %>%
#' #   Label( BMI = "Body-Mass-Index",
#' #         WHtR =  "Waist-Height-Ratio",
#' #         WHtR_1 ="Waist-Height-Ratio",
#' #         bildprof = "Bildungsprofil",
#' #         jahr = "Jahr")
#' #
#' # MobilePayment %>%
#' #  select(Q2_1:Q2_4) %>%
#' #   Label("Computer", "Laptop", "Tablet", "Mobiltelefon" )
#'
Label<- function(data, ...){
    my_labels<- list(...)
    if (length(my_labels) == 0) {
        cat("\\nLabel: Keine Labels gefunden!\\n")
        return(data)
    }else{
        if(is.null(names(my_labels))) {
          cat("\\nLabel: Keine Namen gefunden! Verwende daher names(data)\n" )
          names(my_labels) <-  names(data)[1:length(my_labels)]
        }
        label_data_frame(data, my_labels)
    }
}




#' @rdname CleanUp
#' @export
#' @param labels Labels
label_data_frame <- function(data, labels = NULL) {
  if (is.null(labels)) {
    cat("\nlabel_data_frame: Keine Labels gefunden!\n")
    return(data)
  } else {
    no <- names(data)
    nl <- no %in% names(labels)
    if (sum(nl) > 0) {
      for (n in no[nl])
        attr(data[[n]], "label") <- labels[[n]]
      return(data)
    }
  #  cat("\nlabel_data_frame: Falsche Labels gefunden!\n")
   # print(names(labels))
    return(data)
  }
}




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
