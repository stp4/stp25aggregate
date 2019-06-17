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
  lbl<- list(...)
  if (length(lbl) == 0) {
    message("Label: Keine Labels gefunden!")
    return(data)
  }else{
    if(is.null(names(lbl))) {
      message("Label: Keine Namen gefunden! Verwende daher names(data)" )
      names(lbl) <-  names(data)[1:length(lbl)]
    }
    set_label(data, lbl)
  }
}




#' @rdname get_label
#' @description DeletLabels:  Loeschen des Attributs label
#'
#' @export
 
delet_labels <- function(data){
  for (n in names(data))
    attr(data[[n]], "label") <- NULL
  data
}


#' @rdname get_label
#' @param labels Labels
set_label <- function(data, labels = NULL) {
  if (is.null(labels)) {
    message("Warnung label_data_frame: Keine Labels gefunden!\n")
    return(data)
  } else {
    nms <- names(data)
    nl <- nms %in% names(labels)
    if (sum(nl) > 0) {
      for (n in nms[nl])
        attr(data[[n]], "label") <- labels[[n]]
    }
  }
  data
}

#' @rdname CleanUp
#' @description Intern wenn mit get_label nur die Kopie wiederhergestellt wird
 
label_data_frame  <- function(x, labels) {
  if (all(names(x) == names(labels))) {
    for (i in names(x)) {
      attr(x[[i]], "label") <- labels[[i]]
    }
    x
  }
  else
    set_label(x, labels)
  
}

#' get_label
#' 
#' Labels ohne die unit bei GetLabelOrName weid auch die unit verwendet.
#'
#' @param x  data.drame
#'
#' @return Namend Character String
#' @export
get_label <- function(x) {
  lbl <- lapply(x, attr, "label")
if(length(lbl)==0) return(NULL)
  
  unlabl <- which(sapply(lbl, is.null))
  lbl[unlabl] <- names(lbl[unlabl])
  unlist(lbl)

}

#' @rdname get_label
#' @description  Extrahiert die Labels
#' @param pattern pattern = "[\\._]+"
#' @param replacement Leerzeichen
#' @export
#' @examples
#' 
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

