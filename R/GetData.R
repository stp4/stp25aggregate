#' @name GetData
#' @rdname GetData
#' @title Daten Importieren
#' @description Ladet verschiedene Dateiformate von csv bis sav.  Tabellen im Text-Format koennen direkt gelesen werden.
#' Zurueckgegeben wird ein Hmisc data.frame mit Levels und Labels siehe \link{upData}.
#' \subsection{CSV-Files}{Liest CSV-Files siehe \link{read.csv}   uebergeben werden Trennzeichen, Komma, usw.
#'   }
#' \subsection{Text direkt im R-File}{
#'  Es lassen sich direkt Daten als Text einlesen dabei kann mit \code{Tabel_Expand = TRUE, id.vars = 1:2} gesteuert
#'  werden ob eine Kreuztabelle aufgedroeselt werden soll.
#'   }
#' \subsection{SPSS}{
#' Mit \code{GetData("Dummy.sav", reencode="UTF-8")} lassen sich SPSS-Files einlesen. Bei SPSS
#' Dateien (sav und por) werden leere Labels
#' automatisch bereinigt dH sie werden durch die Number des Faktors ersaetzt. Probleme
#' machen lange Text-Variablen im SPSS-File, die sollten im Falle geloescht werden. Weitere
#' Info unter  \link{spss.get} und \link{read.spss}.
#' Neu ist das  die Default-Option   spss.get() is  um mit haven zu Arbeitenist als Parameter  \code{type=2}
#' zu ?bergeben. Bei hven werden zus?tzlich die Faktoren bereinigt.
#'   }
#' \subsection{LimeSurvy}{
#'  Hier muss eine Liste uebergeben werden die die Filenamen beinhaltet also \code{GetData(list("file.R","file.txt"))}.
#'  Das erste File ist das R-File mit den Labels das zweite die Daten. Weitere moegliche Parameter sind die Zeichencodierung.
#'  }
#' \subsection{RData}{
#'  Hier erfolgt das Einlesen einfach ueber \code{load(File, ...)}, besser ist es daher direkt den Befehl zu verwenden.
#'  }
#'
#' @param File Demo.xla, Demo.sav, Demo.csv  - ist ein Pfad zu einem csv, xls, sav oder Rdata Datensatz oder ein String in Textformat dierekt im R-File.
#' @param na.strings c(NA,9999,"") - Fehlende Werte
#' @param force.numeric logical FALSE
#' @param Tabel_Expand logical FALSE - Tabellen mit haufigkeiten werden als Dataframe im long-Format ausgegeben
#' @param id.vars nur mit Tabel_Expand  - Nummer und Name der ID-Variablen bei Tabel_Expand default ist 1.
#' @param value nur mit Tabel_Expand  - Name der output-variable bei Tabel_Expand.
#' @param Data_info  Data_info = date(),
#' @param sep Lesen der csv- Files = ";",
#' @param quote Lesen der csv- Files = "\"",
#' @param dec  Lesen der csv- Files = ".",
#' @param reencode  UTF-8 = FALSE,
#' @param user_na	If TRUE variables with user defined missing will be read into labelled_spss objects. If FALSE, the default, user-defined missings will be converted to NA.
#' @param ...  Argumente fuer spss und csv. Bei SPSS-Files  kann die Zeichencodierung mit  \code{reencode ="UTF-8"} geaendert werden.
#' @export
#' @examples 
#' 
#'  dat<-GetData("
#' sex treatment control
#' m  2 3
#' f  3 4
#' ",Tabel_Expand = TRUE, id.vars = 1)
#' 
#' xtabs(~sex +value, dat)
#' 
#' 
#' dat<-GetData("
#' sex treatment  neg  pos 
#' f   KG          3   3
#' f   UG          4   5
#' m   KG          5   4
#' m   UG          4   2
#' ",Tabel_Expand = TRUE, id.vars = 1:2, value="befund")
#' 
#' ftable(xtabs(~sex +treatment +befund, dat))

GetData <- function (File = NA,
            na.strings = NULL,
            force.numeric = FALSE,
            Tabel_Expand = FALSE,
            id.vars = 1,
            value = "value",
            Data_info = date(),
            sep = ";",
            quote = "\"",
            dec = ".",
            user_na=FALSE,
            reencode = FALSE,
            ...)
            {
    iconv.data.frame <-
              function(df, from = "UTF8", to = "latin1", ...) {
                df.names <- iconv(names(df), from, to)
                df.rownames <- iconv(rownames(df), from, to)
                df.label <- iconv(Hmisc::label(df), from , to)

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
                #   , labels=     df.label
                Hmisc::upData(data.frame(df.list))
              }


    LimeSurvy <- function(myfiles,
                                    from = "UTF8",
                                    to = "latin1",
                                    ...) {
                source(myfiles[[1]], echo = TRUE)
                df.names   <- iconv(names(data), from, to)
                df.rownames <- iconv(rownames(data), from, to)
                df.label   <-
                  iconv(attributes(data)$variable.labels, from, to)      ##   iconv(Hmisc::label(df),  from , to )
                #    sapply( strsplit(  gsub("[\n\t//[]", "", df_label), "]") ,function(x)  x[1])



                names(df.label) <- df.names
                names(data) <- df.names
                rownames(data) <- df.rownames
                df.list <- lapply(data
                                  , function(x) {
                                    if (any(class(x) == "factor")) {
                                      levels(x)  <- iconv(levels(x), from, to)
                                      x
                                    } else if (any(class(x) == "character")) {
                                      iconv(x, from, to)

                                    } else{
                                      x
                                    }
                                  })


                label_data_frame(data.frame(df.list),
                                 labels = df.label)
              }
    cleanup_NA <-
                function(obj,
                         na.strings = NULL,
                         force.numeric = TRUE) {
                  dimobj <- dim(obj)
                  for (i in 1:dimobj[2]) {
                    x <- obj[[i]]
                    if (!is.null(na.strings)) {
                      x[x %in% na.strings] <- NA
                      modif <- TRUE
                    }
                    if (force.numeric && length(lev <- levels(x))) {
                      x <- factor(x)
                      if (all.is.numeric(levels(x))) {
                        x <- as.numeric(as.character(x))
                        modif <- TRUE
                      }
                    }
                    if (modif)
                      obj[[i]] <- x
                    NULL
                  }
                  obj
                }


    read.text2 <-
                function (Lines,
                          na.strings = c("NA", "na"),
                          sep = "\t",
                          dec = ".") {
                  cleanup <- function(x) {
                    gsub(sep, " ", x)
                  }
                  myData <- read.table(
                    zz <- textConnection(cleanup(Lines)),
                    header = TRUE,
                    dec = dec,
                    na.strings = na.strings
                  )
                  close(zz)
                  myData
                }

    TabelToExpandDataFrame <- function(myData, id.vars, value = "value") {
      

      
                expand.dft <-
                  function(x,
                           na.strings = "NA",
                           as.is = FALSE,
                           dec = ".") {
                    #-- http://wiki.stdout.org/rcookbook/Manipulating%20data/Converting%20between%20data%20frames%20and%20contingency%20tables/
                    # Take each row in the source data frame table and replicate it
                    # using the Freq value
                    myData <- sapply(1:nrow(x),
                                 function(i)
                                   x[rep(i, each = x$Freq[i]),],
                                 simplify = FALSE)

                    # Take the above list and rbind it to create a single myData
                    # Also subset the result to eliminate the Freq column
                    myData <- subset(do.call("rbind", myData), select = -Freq)

                    # Now apply type.convert to the character coerced factor columns
                    # to facilitate data type selection for each column
                    for (i in 1:ncol(myData)) {
                      myData[[i]] <-
                        type.convert(
                          as.character(myData[[i]]),
                          na.strings = na.strings,
                          as.is = as.is,
                          dec = dec
                        )
                    }
                    myData
                  }
                
                
                # library(reshape2)
                if (!is.data.frame(myData))
                  myData <-
                    read.text2(myData)  # nur wenn die Funktion dierekt aufgerufen wird moeglich
                if (!is.numeric(id.vars))
                  id.vars <- which(names(myData) %in% id.vars)
                myDataMatrix <- as.matrix(myData[,-id.vars])
                if (length(id.vars) == 1) {
                  dimnames(myDataMatrix)[[1]] <- myData[, 1]
                  myData2 <-
                    expand.dft(as.data.frame(as.table(myDataMatrix), stringsAsFactors = TRUE))
                  colnames(myData2)[2] <- value
                }
                else {
                  dimnames(myDataMatrix)[[1]] <- apply(myData[, id.vars], 1, paste,
                                                   collapse = "+")
                  myData2 <-
                    expand.dft(as.data.frame(as.table(myDataMatrix), stringsAsFactors = TRUE))
                  colnames(myData2)[2] <- value
                  
                  myData2 <-
                    cbind(reshape2::colsplit(myData2[, 1], "\\+", names(myData)[id.vars]),  myData2)
                }

                myData2 <-
                  as.data.frame(lapply(myData2, function(x)
                    if (is.character(x))
                      factor(x)
                    else
                      x))
                if(length(id.vars)==1)  {
                  names(myData2)[1] <-names(myData)[id.vars]
                }
              
                
                myData2
              }

              #-- Begin der Funktion -------------------------------------------------
    myData <- data.frame(NULL)
    file_info <- "Text "
    cat("\n\nFile: \n", File, " ", class(File))

    # 15.11.2013 09:47:26
    if (is.list(File)) {
                cat("\n\nLimeSurvy\n")
                myData <- LimeSurvy(File)
    }
    else if (length(grep("\n", File)) > 0) {
                cat("\n\nread.text2\n")
                myData <- read.text2(File, ...)
    }
    else {
                # sonstige Datafiles
          if (file.exists(File)) {
                  file_info <- file.info(File)[c(1, 4, 5)]
                  cat("\n\nfile_info\n")
                  ext <- tolower(tools::file_ext(File))
                  cat("\n\next\n")
                  myData <- switch(
                    ext,
                    sav = CleanUp_factor(haven::read_sav(File, user_na = user_na)),
                    por = CleanUp_factor(haven::read_por(File, user_na = user_na)),
                    csv = read.csv(File,sep = sep,quote = quote,dec = dec,...),
                    rdata = load(File, parent.frame(n = 1)),
                    stop("Unknown extension '.", ext, "'", call. = FALSE)
                  )

                  if (reencode)
                    myData <- cleanup_names_encoding(myData)
                    Data_info <- paste0(
                            rownames(file_info)[1],
                            " (",file_info$size,
                            "KB), ",  file_info$ctime,
                            ", N=", nrow(myData),
                            ", Var=", ncol(myData),
                            ", Missing=", sum(is.na(myData))
                  )

                  Text(Data_info)
            }# --if else file exist
            else {
                  cat(paste("Kein File mit namen: ", File, "vorhanden"))
        }
      } #Elsw sonstige Files

              #-- myData ist jetzt geladen
      nam <- names(myData)
              #-- doppelte .. entfernen
      names(myData) <- gsub("\\.$", "", gsub("\\.+", ".", nam), perl = T)

      if ((!is.null(na.strings)) | force.numeric) {
                myData <- cleanup_NA(myData, na.strings, force.numeric)
              }
      if (Tabel_Expand) {
                myData <-
                  TabelToExpandDataFrame(myData, id.vars = id.vars, value = value, ...)
      }
      comment(myData) <- Data_info
      myData
}
