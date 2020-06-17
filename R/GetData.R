#' @name GetData
#' @rdname GetData
#' @title Daten Importieren
#' @description Ladet verschiedene Dateiformate von csv bis sav.  Tabellen im Text-Format koennen direkt gelesen werden.
#' Zurueckgegeben wird ein   data.frame mit Levels und Labels siehe \link{upData}.
#' \subsection{CSV-Files}{Liest CSV-Files siehe \link{read.csv}   uebergeben werden Trennzeichen, Komma, usw.
#'   }
#' \subsection{xls}{
#' Verwendet readxl::read_excel
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
#' zu ?bergeben. Bei haven werden zusetzlich die Faktoren bereinigt.
#'   }
#' \subsection{LimeSurvy}{
#'  Hier muss eine Liste uebergeben werden die die Filenamen beinhaltet 
#'  also \code{GetData(list("file.R","file.txt"))}.
#'  Das erste File ist das R-File mit den Labels das zweite die Daten. 
#'  Weitere moegliche Parameter sind die Zeichencodierung.
#'  }
#' \subsection{RData}{
#'  Hier erfolgt das Einlesen einfach ueber \code{load(File, ...)}, besser ist es daher direkt den Befehl zu verwenden.
#'  }
#'
#' @param path Demo.xla, 
#' Demo.sav, 
#' Demo.csv  - ist ein Pfad zu einem csv, xls, sav oder Rdata Datensatz 
#' oder ein String in Textformat dierekt im R-File.
#' @param na.strings c(NA,9999,"") - Fehlende Werte
#' @param force.numeric logical FALSE
#' @param Tabel_Expand logical FALSE - Tabellen mit haufigkeiten werden als Dataframe im long-Format ausgegeben
#' @param id.vars nur mit Tabel_Expand  - Nummer und Name der ID-Variablen bei Tabel_Expand default ist 1.
#' @param value nur mit Tabel_Expand  - Name der output-variable bei Tabel_Expand.
#' @param Data_info  Data_info = date(),
#' @param sep,quote,dec Lesen der csv- Files = ";", = "\"", ".",
#' @param sheet,skip,range an readxl::read_excel
#' @param reencode  UTF-8 = FALSE,
#' @param user_na	If TRUE variables with user defined missing will be read into labelled_spss objects. If FALSE, the default, user-defined missings will be converted to NA.
#' @param output Text Info zu File
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
#' 
#' 
#' 
#' 
#'  dat1 <- rockchalk::genCorrelatedData(N=250, 
#' means=c(100,100),
#' sds=c(30,20), 
#' rho=0.0,  stde = 7, 
#' beta=c(1.1, 2.4, 4.1, 0))
#' 
#' #summary(dat1)
#' #m1 <- lm(y ~ x1 + x2, data=dat1)
#' 
#' 
#' 
#' 
#' #' 
#' #' #Exporting data is handled with one function, export():
#' 
#' library("rio")
#' 
#' export(mtcars, "mtcars.csv") # comma-separated values
#' export(mtcars, "mtcars.rds") # R serialized
#' export(mtcars, "mtcars.sav") # SPSS
#' #A particularly useful feature of rio is the ability to import from and export to #compressed (e.g., zip) directories, saving users the extra step of compressing a large #exported file, e.g.:
#' 
#' export(mtcars, "mtcars.tsv.zip")
#' #As of rio v0.5.0, export() can also write multiple data farmes to respective sheets of #an Excel workbook or an HTML file:
#' 
#' export(list(mtcars = mtcars, iris = iris), file = "mtcars.xlsx")
#' 
#' 
#' x <- import("mtcars.csv")
#' y <- import("mtcars.rds")
#' z <- import("mtcars.sav")

GetData <- function(data_file,
                    raw_data = NULL,
                    output = TRUE,
                    ...) {
  if (is.null(raw_data)) {
    data <- get_data(data_file, ...)
  }
  else if (!file.exists(raw_data)) {
    data <- get_data(data_file, ...)
    save(data, file = raw_data)
  }
  else{
    load(raw_data)
  }
  
  
  if (output) {
    stp25output::Text(comment(data))
  }
  
  data
}

#' cleanup_NA
#' 
#' 
#'
#' @param obj data.frame
#' @param na.strings na als character
#' @param force.numeric alles zu Nummern
#'
#' @return data.frame
cleanup_NA <-
    function(obj,
             na.strings = NULL,
             force.numeric = FALSE) {
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
  
#' Read Text Lines
#'
#' @param string  character string
#' @param na.strings 	  a character vector of strings which are to be interpreted as NA values.
#' @param sep 	  the field separator character. 
#' @param dec  the character used in the file for decimal points.
#' @param stringsAsFactors 
#'
#' @return data.frame
read.text2 <-
    function (string,
              na.strings = c("NA", "na"),
              sep = "\t",
              dec = ".",
              stringsAsFactors = TRUE) {
      
      data <- read.table(
        zz <- textConnection(gsub(sep, " ", string)),
        header = TRUE,
        dec = dec,
        na.strings = na.strings,
        stringsAsFactors = stringsAsFactors)
      close(zz)
      data
    }  





#' Helper for TabelToExpandDataFrame
#' 
#' @noRd
#' 

expand.dft <-
  function(x,
           na.strings = "NA",
           as.is = FALSE,
           dec = ".") {
    #-- http://wiki.stdout.org/rcookbook/Manipulating%20data/Converting%20between%20data%20frames%20and%20contingency%20tables/
    # Take each row in the source data frame table and replicate it using the Freq value
    data <- sapply(1:nrow(x),
                   function(i)
                     x[rep(i, each = x$Freq[i]),],
                   simplify = FALSE)
    
    # Take the above list and rbind it to create a single data
    # Also subset the result to eliminate the Freq column
    data <-
      subset(do.call("rbind", data), select = -Freq)
    
    # Now apply type.convert to the character coerced factor columns
    # to facilitate data type selection for each column
    for (i in 1:ncol(data)) {
      data[[i]] <-
        type.convert(
          as.character(data[[i]]),
          na.strings = na.strings,
          as.is = as.is,
          dec = dec
        )
    }
    data
  }

#' Tabel To Expand Data Frame
#'
#' @param data 
#' @param id.vars 
#' @param value 
#' @param na.strings 
#' @param as.is 
#' @param dec 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' dat <- TabelToExpandDataFrame("
#' sex treatment  neg  pos
#' f   KG          3   3
#' f   UG          4   5
#' m   KG          5   4
#' m   UG          4   2
#' ",
#' id.vars = 1:2,
#' value = "befund")
#' 
#' xdat <- xtabs( ~ befund + sex + treatment, dat)
#' Wide(as.data.frame(xdat),
#'      befund ,
#'      Freq)
TabelToExpandDataFrame <-
  function(data, 
           id.vars, 
           value = "value",
           na.strings = "NA",
           as.is = FALSE,
           dec = ".") {
  
    if (is.character(data))
      data <- read.text2(data)  # nur wenn die Funktion dierekt aufgerufen wird moeglich
    
    
    if (!is.numeric(id.vars))
      id.vars <- which(names(data) %in% id.vars)
    
    dataMatrix <- as.matrix(data[, -id.vars])
    
    if (length(id.vars) == 1) {
      dimnames(dataMatrix)[[1]] <- data[, 1]
      data2 <-
        expand.dft(as.data.frame(as.table(dataMatrix), 
                                 stringsAsFactors = TRUE),
                   na.strings, as.is, dec
                   )
      colnames(data2)[2] <- value
    }
    else {
      dimnames(dataMatrix)[[1]] <- apply(data[, id.vars], 1, paste,
                                           collapse = "+")
      data2 <-
        expand.dft(as.data.frame(as.table(dataMatrix), 
                                 stringsAsFactors = TRUE),
                   na.strings, as.is, dec)
      colnames(data2)[2] <- value
      
      data2 <-
        cbind(reshape2::colsplit(data2[, 1], "\\+", names(data)[id.vars]),  data2)
    }
    
    data2 <-
      as.data.frame(lapply(data2, function(x)
        if (is.character(x))
          factor(x)
        else
          x))
    if (length(id.vars) == 1)  {
      names(data2)[1] <- names(data)[id.vars]
    }
    
    data2
  }



LimeSurvy <- function(path,
                        from = "UTF8",
                        to = "latin1") {
    source(path[[1]], echo = TRUE)
    df.names   <- iconv(names(data), from, to)
    df.rownames <- iconv(rownames(data), from, to)
    df.label   <-
      iconv(attributes(data)$variable.labels, from, to)
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
    
    
    set_label(data.frame(df.list),
              labels = df.label)
  }
  
  
get_data <- function (path = NA,
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
            sheet = 1, 
            range = NULL, 
            skip = 0, 
            ...)
            {

  data <- data.frame(NULL)
  file_info <- "Text "

  
  # 15.11.2013 09:47:26
  if (is.list(path)) {
    cat("\n\nLimeSurvy\n")
    data <- LimeSurvy(path)
  }
  else if (length(grep("\n", path)) > 0) {
    cat("\n\nread-text\n")
    data <- read.text2(path, dec=dec)
  }
  else {
    if (file.exists(path)) {
      cat("\n\nread-file\n")
      file_info <- file.info(path)[c(1, 4, 5)]
      ext <- tolower(tools::file_ext(path))
 
      data <- switch(
        ext,
        sav = CleanUp_factor(
                  haven::read_sav(
                    path,
                    user_na = user_na)),
        por = CleanUp_factor(
                  haven::read_por(
                    path,
                    user_na = user_na)),
        xlsx = clean_names(
                  readxl::read_excel(
                    path,
                    sheet = sheet,
                    skip = skip,
                    range = range,
                    ...)),
        csv = clean_names(
                  readr::read_csv(
                            path,
                          sep = sep,
                          skip = skip,
                          quote = quote,
                          dec = dec,
                          ...)),
        rdata = load(path, 
                     parent.frame(n = 1)),
        stop("Unknown extension '.", ext, "'", call. = FALSE)
      )
      
      if (reencode)
        data <- cleanup_names_encoding(data)
      
      Data_info <- paste0(
        rownames(file_info)[1],
        " (",
        file_info$size,
        "KB), ",
        file_info$ctime,
        ", N=",
        nrow(data),
        ", Var=",
        ncol(data),
        ", Missing=",
        sum(is.na(data))
      )
      
      
    }# --if else file exist
    else {
      stop(note, "Kein path: ", path, "vorhanden!")
    }
  } #Elsw sonstige Files
  
  if ((!is.null(na.strings)) | force.numeric) {
    data <- cleanup_NA(data, na.strings, force.numeric)
  }
  if (Tabel_Expand) {
    data <-
      TabelToExpandDataFrame(data, id.vars = id.vars, value = value, ...)
  }
  
  comment(data) <- Data_info
  data
}

 
