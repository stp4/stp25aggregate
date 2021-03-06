% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upData2.R
\name{upData2}
\alias{upData2}
\title{upData2}
\usage{
upData2(
  data,
  labels = NULL,
  reencode = FALSE,
  from = "UTF8",
  to = "latin1",
  data_dict = NULL,
  factor_sep = ";",
  names_data_dict = c("names", "labels", "levels"),
  ...
)
}
\arguments{
\item{data}{Daten als Data.frame}

\item{labels}{Labels Vekror mit Names labels=c(a="Alpha", s="sex")}

\item{reencode}{FALSE or TRUE}

\item{from}{für reencode from = "UTF8"}

\item{to}{für reencode to = "latin1",}

\item{data_dict}{data.frame mit names, labels, levels}

\item{factor_sep}{seperator wen levels vergeben werden default=";"}

\item{names_data_dict}{brauch nicht geändert werden}

\item{...}{extra arguments not used}
}
\description{
Eine Art von Copie upData mit der
Funktion von label_data_frame. Mit UpData2 lassen
sich zB mit einem DataDictionary in Form eines
data.frames die Labels und Levels von Faktoren dokomentieren.

Faktor-Levels vorher: level (Tomate, Bohne, Gurke)  wird
mit "Tomato;;Cucumber" zu
level( Tomato, NA, Cucumber)
}
\examples{

data<- data.frame(g= gl(2, 8, labels = c("Control", "Treat")),
a=rpois(16, 5),
b=rep(1:2,8),
c=rnorm(16))

data$d<-rep(c("a", "b", "c"), 6)[1:16]
str(upData2(data, labels = c(g="Gruppe" , a="A", b="B") ))


datadir <- GetData("
                   names labels    levels
                   g     Gruppe    NULL
                   a     A         numeric
                   b     Sex       male;female
                   d     DNA       a;b
                   c     C         NULL
                   ")

data<-upData2(data, datadir)

APA2(~., data)
}
