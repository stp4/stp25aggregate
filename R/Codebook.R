#' Extrahier die Daten-Struktur


#' @rdname upData2
#' @description Die Funltion \code{Codebook()} Extrahier die Daten-Struktur
#' @export
Codebook<- function(data){
  lbs <- get_label(data)
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