#' Label
#'
#' Setzen des attr(data, "label")
#' @param data data.frame
#' @param ... Labels in der Form a="Hallo, b="Welt"
#' @export
#' @examples
#'
#' df <- data.frame(
#' BMI=c(1,2,3,1,2,3),
#' WHtR= gl(2,3, labels =c("Amy", "Bob")),
#' WHtR_1=c(9,7,6,8,6,9),
#' bildprof=c(6,7,8,5,6,7)
#' )
#'
#' DF<-
#'   Label(df, BMI = "Body-Mass-Index",
#'         WHtR =  "Waist-Height-Ratio",
#'         WHtR_1 ="Waist-Height-Ratio"
#'   )
#'
#' DF$BMI<- units::set_units(DF$BMI, kg/m2)
#'
#' get_label(DF)
#' get_label(DF, include.units=TRUE)
#' DF<- set_label(DF, c(bildprof = "Bildungsprofil"))
#' get_label(DF)
#' DF<- delet_labels(DF)
#' get_label(DF)
#'
#' DF<- wrap_label(DF)
#' get_label(DF)
#' 
Label <- function(data, ...) {
  lbl <- list(...)
  if (length(lbl) == 0) {
    message("Label: Keine Labels gefunden!")
    return(data)
  } else{
    if (is.null(names(lbl))) {
      message("Label: Keine Namen gefunden! Verwende daher names(data)")
      names(lbl) <-  names(data)[1:length(lbl)]
    }
    set_label(data, unlist(lbl))
  }
}


#' @rdname Label
#' @description delet_labels:  Loeschen aller Attributs label
#'
#' @export
delet_labels <- function(data) {
  for (n in names(data))
    attr(data[[n]], "label") <- NULL
  data
}


#' @rdname Label
#' @description set_label:  Setzen  der Attributs label
#' @param labels Labels als Character-String mit Namen c(a="Hallo, b="Welt")
#'
#' @export
set_label <- function(data, labels = NULL) {
  if (is.null(labels)) {
    message("Warnung set_label: Keine Labels gefunden!\n")
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


#' @rdname Label
#' @description get_label und GetLabelOrName:  Abrufen  der Attributs label
#' @export
#' 
get_label <- function(data, 
                      include.units = FALSE
                     # , include.names=options()$stp25$include_names
                      ) {
  lbl <- lapply(data, attr, "label")
  if (length(lbl) == 0)
    return(NULL)
  
  unlabl <- which(sapply(lbl, is.null))
  # if (!is.null(include.names)){
  #   lbl[unlabl] <- ""
  #   lbl <- unlist(lbl) 
  #   lbl_nams <- names(lbl)
  #   lbl <- paste0(lbl_nams, ": ", lbl)
  #   names(lbl) <- lbl_nams
  # }else{
  lbl[unlabl] <- names(lbl[unlabl])
  lbl <- unlist(lbl) 
#  }
  
  if (include.units) {
    
    is_units <- sapply(data, function(z)
     # inherits(z, "units")
      any(names(attributes(z)) %in% "units")
      )
    if (any(is_units)) {
      
      lbl_nams <- names(lbl)
      
      lbl_units <-
        sapply(data, function(z)
          if ( 
            #inherits(z, "units")
            any(names(attributes(z)) %in% "units")
            )
            paste0(" [", as.character(attr(z, "units")), "]")
          else
            "")
      lbl <-  paste0(lbl, lbl_units)
      names(lbl) <- lbl_nams
    }
  }
  lbl
}


#' @rdname Label
#' @description wrap_label kuerzt die Labels fur Grafiken.
#' @export
#' 
#' @param x data.frame or string
#' @param width  laenge 20
#' @param sep default newline 
#' @param pattern,replacement an gsub
#' 
wrap_label <-
  function(x,
           width = 20,
           sep = NULL,
           pattern = "_",
           replacement = " ")
  {
    if (is.data.frame(x)) {
      lvl <- .wrap_string(get_label(x), width, sep, pattern, replacement)
      names(lvl) <- names(x)
      set_label(x, lvl)
    }
    else{
      .wrap_string(x, width, sep, pattern, replacement)
    }
  }

.wrap_string <- function(x, width, sep, pattern, replacement) {
  x <- trimws(x)
  x <- gsub("\r?\n|\r", " ", x)
  if (!is.null(pattern))
    x <- gsub(pattern, replacement, x)
  
  if (is.null(sep))
    stringr::str_wrap(x, width = width)
  else
    gsub("\n", sep, stringr::str_wrap(x, width = width))
  
  
}

#' @rdname Label
#' @param include.units Einheiten 
#' @export
#'
GetLabelOrName <- function(data, include.units = TRUE) {
  get_label(data, include.units)
}

#' @rdname Label
#' @param data data.frame
#'
#' @param labels labels als named vector
#' 
#' @description  Intern wenn mit get_label nur die Kopie wiederhergestellt wird.
#'
label_data_frame  <- function(data, 
                              labels) {
  if (all(names(data) %in% names(labels))) {
    for (i in names(data)) {
      attr(data[[i]], "label") <- labels[[i]]
    }
    data
  }
  else
    set_label(data, labels)
}
