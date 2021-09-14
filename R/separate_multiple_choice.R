#' @rdname transform2
#' @description \code{separate_multiple_choice()} Aufdröseln vom Mehrfachantworten, die Funktion transformiert einen String mit Trennzeichen
#' zu einem Multi-Set mit 0 und 1. (Separate multiple choice)
#' @param x Vektor ist entweder ein Character oder eine zahl
#' Bei Zahlen gehen nur 1 bis 11
#' @param sep  wichtig ist das trennzeichen
#' @param prafix neue Variablen Name fuer die ausgabe default ist der Name vom Input
#' @param labels,levels Outut nimmeric oder factor
#' @param into  brauch nicht gesetzt werden entspricht der Anzahl an levels
#' @param label Labels
#' @return data.frame mit 0 1 Kodierung und den Fragen als Labels
#' @export
#'
#' @examples
#' dat <-  data.frame(Q1=c(134,NA,35,134,5,24),
#'                    Q2=c(
#'
#'                      "Alm Dudler, Essig, Cola",
#'                      NA,
#'                      "Cola, Holer",
#'                      "Alm Dudler, Cola, Essig",
#'                      "Holer",
#'                      "Bier, Essig"))
#' #dat
#'
#'
#' dat<- cbind(dat,separate_multiple_choice(dat$Q2))
#'
#' dat<- cbind(dat,separate_multiple_choice(dat$Q1,
#'                                          label=c(
#'                                            "Alm Dudler","Bier", "Cola", "Essig", "Holer") ))
#' names(dat)<- GetLabelOrName(dat)
#' dat
#' x<-data.frame(A=c(15911,261011,3711,4811))
#' separate_multiple_choice(x$A)
separate_multiple_choice <- function(x,
                                     sep = ", ",
                                     labels = c("ja", "nein"),
                                     levels = 1:0,
                                     prafix =  NULL,
                                     into = NULL,
                                     label = NULL,
                                     na.strings = NULL) {
  # callingFun = as.list(sys.call(-1))[[1]]
  # calledFun = as.list(sys.call())[[1]]
  if (!is.null(na.strings))
    x[which(x == na.strings)] <- NA
  
  if (is.null(prafix))
    prafix <- paste0(gsub("^.*\\$", "", deparse(substitute(x))), "_")
  
  
  is_numeric_x <- FALSE
  
  if (is.numeric(x)) {
    is_numeric_x <- TRUE
    x <- as.character(x)
    x <- gsub("10", "a", x)
    x <- gsub("11", "b", x)
    
    x <- gsub("(.)\\B", "\\1,", x)
    sep <- ","
  }
  
  
  prafix <- gsub("[^[:alnum:]_]", "", prafix)
  #leere levels nicht  entfernen sondern behalten
  x <- as.character(x)
  x[is.na(x)] <- "XX_9999"
  x <- factor(x)
  
  # separate braucht die Anzahl an Levels
  unique_elements <-
    unique(unlist(stringr::str_split(x, sep)), use.names = FALSE)
  #print(unique_elements)
  if (is.null(into))
    into <- paste0("M", 1:length(unique_elements))
  
  # id ist zur kontrolle und name xxxx damit es zu keinen konflikten kommt
  res <-  data.frame(id = 1:length(x), xxxx = x)
  
  res <-
    tidyr::separate(res,
                    "xxxx",
                    into = into,
                    sep = sep,
                    remove = F)  ## Aufsplitten
  res <-
    na.exclude(tidyr::gather(res, q2 , val, -id, -xxxx))   ## breit zu lang
  
  res <- tidyr::spread(dplyr::mutate(res, q2 = 1), val, q2)
  res[-1:-2][is.na(res[-1:-2])] <- 0
  
  if (any(names(res) == "XX_9999")) {
    res[which(res$XX_9999 == 1),-1] <- NA
    res <- res[,-ncol(res)]
  }
  
  lbl <- names(res)[-1:-2]
  names(res)[-1:-2] <- names(lbl) <- paste0(prafix, 1:length(lbl))
  if (is.null(labels))
    res <- res[-1:-2]
  else
    res <- dapply2(res[-1:-2], function(z)
      factor(z, levels, labels))
  
  if (!is.null(label) & is_numeric_x) {
    lbl <- gsub("a", "10", lbl)
    lbl <- gsub("b", "11", lbl)
    
    x <- as.numeric(lbl)
    y <- 1:length(label)
    used <- intersect(x, y)
    not_used <- setdiff(y, used)
    
    names(res) <- paste0(prafix, used)
    for (i in   not_used) {
      res[paste0(prafix, i)] <- NA
    }
    
    
    names(label) <- paste0(prafix, 1:length(label))
    
    cf <- names(res)
    res[order(nchar(cf), cf)]
    
  res<-   set_label(res, label)
    
  } else{
  res <-  set_label(res, lbl)
  }
}