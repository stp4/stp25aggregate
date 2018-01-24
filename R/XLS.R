#' Nummernindex aus Excel - Spaltenbezeichnung
#'
#' @description  Extrahiert aus Buchstaben die Spaltennummer
#' @author Wolfgang Peter
#' @param ... liste mit den Spaltennamen A:BB
#' @param myLetters nicht zum ändern
#' @seealso \link{strsplit}
#' @export
#' @examples
#'
#' #strsplit("A:V", "\\:")
#' XLS(a, B)
#' XLS(a, B, c:f, g:h,i, r:z)
#' XLS(A:Z)

XLS <- function(...,
                myLetters = c(LETTERS, unlist(lapply(LETTERS, function(abc)
                  paste0(abc, LETTERS))))) {
  letter_num <- function(ltr) {
    which(myLetters %in% ltr)
  }
  ltr <- toupper(as.character(sys.call())[-1])

  xrange <- grep("\\:", ltr)
  n <- 0
  if (length(xrange)) {
    for (i in 1:length(xrange)) {
      x <- xrange[i]
      posn <- xrange[i] + n - i + 1
      mltr <- unlist(strsplit(ltr[posn], "\\:"))
      myRange <- myLetters[letter_num(mltr[1]):letter_num(mltr[2])]
      ltr <- append(ltr, myRange, after = posn)
      ltr <- ltr[-posn]
      n <- n + length(myRange)
    }
  }
  letter_num(ltr)
}
