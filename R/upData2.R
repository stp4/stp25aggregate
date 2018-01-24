#' upData2
#' @description Eine Art von Copie upData mit der
#' Funktion von label_data_frame. Mit UpData2 lassen
#' sich zB mit einem DataDictionary in Form eines
#' data.frames die Labels und Levels von Faktoren dokomentieren.
#'
#' Faktor-Levels vorher: level (Tomate, Bohne, Gurke)  wird
#' mit "Tomato;;Cucumber" zu
#' level( Tomato, NA, Cucumber)
#'
#' @param data Daten als Data.frame
#' @param labels  Labels Vekror mit Names labels=c(a="Alpha", s="sex")
#' @param reencode FALSE or TRUE
#' @param from für reencode from = "UTF8"
#' @param to für reencode to = "latin1",
#' @param data_dict data.frame mit names, labels, levels
#' @param factor_sep seperator wen levels vergeben werden default=";"
#' @param names_data_dict brauch nicht geändert werden
#' @param ... extra arguments not used
#' @export
#' @examples
#'
#' data<- data.frame(g= gl(2, 8, labels = c("Control", "Treat")),
#' a=rpois(16, 5),
#' b=rep(1:2,8),
#' c=rnorm(16))
#'
#' data$d<-rep(c("a", "b", "c"), 6)[1:16]
#' str(upData2(data, labels = c(g="Gruppe" , a="A", b="B") ))
#'
#'
#' datadir <- GetData("
#'                    names labels    levels
#'                    g     Gruppe    NULL
#'                    a     A         numeric
#'                    b     Sex       male;female
#'                    d     DNA       a;b
#'                    c     C         NULL
#'                    ")
#'
#' data<-upData2(data, datadir)
#'
#' APA2(~., data)
upData2 <- function(data,
                    labels = NULL,
                    reencode = FALSE,
                    from = "UTF8" ,
                    to = "latin1",
                    data_dict = NULL,
                    factor_sep = ";",
                    names_data_dict = c("names",  "labels",  "levels"),
                    ...) {
is_tbl <- dplyr::is.tbl(data)

  data <- data.frame(data)
  NAs_rm <-  function(x) {
    leere_char <- which(x == "")
    if (length(leere_char) > 0)
      x[-leere_char]
    else
      x

  }
  #-- Putzen ----------------------------------------------
  if (reencode) {
    data <- cleanup_names_encoding(data, from, to)
    data <- cleanup_factor_levels_encoding(data, from, to)
    data <- cleanup_typ_character_encoding(data, from, to)
    if (is.null(labels))
      labels <- iconv(GetLabelOrName(data), from , to)
  }

  #-- fehler Abfanagen
  if (is.matrix(labels) | is.data.frame(labels)) {
    data_dict <- labels
    labels <- NULL
  }

  if (!is.null(data_dict)) {
    #print(labels)

    #--  Factor-Levels ------------------------------------
    if (any(colnames(data_dict) %in% names_data_dict[3])) {
      Wertelabels <- as.character(data_dict[, names_data_dict[3]])
      Wertelabels <- stringr::str_split(Wertelabels, factor_sep)
      #leerzeichen
      Wertelabels <-  sapply(Wertelabels, function(x)
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x))
      names(Wertelabels) <- as.character(data_dict[, names_data_dict[1]])

  valid_names <-base::intersect(names(data),  names(Wertelabels))

  Wertelabels<- Wertelabels[valid_names]

      for (i in names(Wertelabels)) {
        lbl <- Wertelabels[[i]]
        # löschen von Leerzellen also A;;C;D = zweiter level NA
lbl[which(lbl =="") ] <- NA
print(lbl)
        lng <- length(lbl)

        #  nur wenn mehr als ein eintrag
        if (lng > 1) {
          if (is.numeric(data[, i])) {
            mini <- min(data[, i], na.rm = TRUE)
            if (mini < 1) {
              data[, i] <- data[, i] - (mini - 1)
            }
            data[, i] <-
              factor(data[, i], 1:lng, lbl)
          }
          else if (is.factor(data[, i])) {
          #  data[, i] <- as.numeric(data[, i])
            old_lvl <- levels(data[, i])
            # neu und alt gleiche elemente also eine andere Reihenvolge
            # alt a, b, c
            # neu b, c, a
            if(setequal(old_lvl, lbl)){
             # Text("UpData:", names(data[i]), " Change Levels from: ",
             #      paste(old_lvl, collapse="; "),
             #      " to: ",paste(lbl, collapse="; ") )
              data[, i] <- factor(data[, i], lbl)}
            else if (length(old_lvl) == length(lbl)){
              Text("UpData:", names(data[i]), "Levels from: ",
                   paste(old_lvl, collapse="; "),
                   " to: ",paste(lbl, collapse="; ") )
              data[, i] <- factor(data[, i], old_lvl, lbl)}
            else warnings( "Updata2 mit DataDict neu und alt Levalels sind nicht  gleiche lang!")

          } else if (is.character(data[, i])) {
            data[, i] <-
              factor(data[, i], lbl)

          }

          else{
            NULL
          }
        }

        else if (lng==1 & lbl == "factor") {
          data[, i] <- factor(data[, i])
        }
        else if(lng==1 & lbl=="numeric"){
          cat("\nin numeric", class(data[, i]), "\n")
          if(!is.numeric(data[, i]))
          data[, i] <- as.numeric( data[, i])
        } else{
          NULL
        }
      }
    }
    #-- Name Beschriftung  Wertelabels

    #-- Labels --------------------------------------------

    if (any(colnames(data_dict) %in% names_data_dict[2])){
    labels <- as.character(data_dict[, names_data_dict[2]])
    names(labels) <- as.character(data_dict[, names_data_dict[1]])
    labels <- NAs_rm(labels)
    data <- label_data_frame(data, labels)}
  }

  #-- Labels ohne Code_Book ---------------------------
  if (!is.null(labels))
    data <- label_data_frame(data, labels)

  if (is_tbl) tibble::as_tibble(data)
  else data
}

#' Extrahier die Daten-Struktur


#' @rdname upData2
#' @description Die Funltion \code{Codebook()} Extrahier die Daten-Struktur
#' @export
Codebook<- function(data){
  lbs <- GetLabelOrName(data)
  lvl<- sapply(data,
               function(x) {
                 if(is.factor(x)){
                   lvl <- levels(x)
                   if(length(grep(";" , lvl)) != 0 ) "Warning: ; in levels gefunden"
                   else
                     paste0(lvl, collapse=";")
                 }
                 else  class(x)[1]

               })
  cbind(names=names(lbs),
        labels=lbs  ,
        levels=as.vector(lvl) )

}
