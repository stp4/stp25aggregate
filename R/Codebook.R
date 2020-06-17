#' Codebook
#' 
#' Extrahier die Daten-Struktur
#' @param data data.frame
#'
#' @param abbreviate Abkuerzen del Levels
#'
#' @export
Codebook <-
  function(data, abbreviate = TRUE) {
    n <- nrow(data)
    lbs <- get_label(data)
    lvl <- sapply(data,
                  function(x) {
                    if (is.factor(x)) {
                      lvl <- levels(x)
                      if (abbreviate)
                        lvl <- abbreviate(lvl, 
                                          use.classes = FALSE)
                      if (length(grep("\\|" , lvl)) != 0)
                        "Warning: ; in levels gefunden"
                      else
                        paste0(nlevels(x), " levels ", 
                               paste0(' "', 
                                      lvl, '"', 
                                      collapse = ','))
                    }
                    else
                      class(x)[1]
                    
                  })
    
    n_data <- sapply(data,
                     function(x) {
                       length(na.omit(x))
                     })
    
    stat_data <- sapply(data,
                        function(x) {
                          if (is.factor(x))
                            paste(table(x), 
                                  collapse = "|")
                          else if (is.character(x))
                            "character"
                          else
                            paste(
                              fivenum(
                                signif(as.numeric(x), 2), 
                                na.rm = TRUE),
                              collapse = "; ")
                        })
    data.frame(
      names = names(lbs),
      labels = lbs,
      levels = as.vector(lvl),
      stat = stat_data,
      missing = (n - n_data),
      n = n_data
    )
  }

 
