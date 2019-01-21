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


# wegen Hmist dort gibt es die Funktion Labels
#' @rdname CleanUp
#' @export
#' 
SetLabels<- function(x, ...){ Label(x, ...) }

#' @rdname CleanUp
#' @description DeletLabels:  Loeschen des Attributs label
#'
#' @export
 
DeletLabels <- function(data){
  for (n in names(data))
    attr(data[[n]], "label") <- NULL
  data
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


