% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform2.R
\name{transform2}
\alias{transform2}
\alias{mutate2}
\alias{separate_multiple_choice}
\title{transform2}
\usage{
transform2(data, ...)

mutate2(data, ...)

separate_multiple_choice(
  x,
  sep = ", ",
  labels = c("ja", "nein"),
  levels = 1:0,
  prafix = NULL,
  into = NULL,
  label = NULL,
  na.strings = NULL
)
}
\arguments{
\item{data}{Daten}

\item{...}{alles an transform}

\item{x}{Vektor ist entweder ein Character oder eine zahl
Bei Zahlen gehen nur 1 bis 11}

\item{sep}{wichtig ist das trennzeichen}

\item{labels, levels}{Outut nimmeric oder factor}

\item{prafix}{neue Variablen Name fuer die ausgabe default ist der Name vom Input}

\item{into}{brauch nicht gesetzt werden entspricht der Anzahl an levels}

\item{label}{Labels}
}
\value{
data.frame mit 0 1 Kodierung und den Fragen als Labels
}
\description{
Kopie von \link{transform} wobei die Labels
mit upData2 ergaenzt werden. ist

\code{separate_multiple_choice()} Aufdröseln vom Mehrfachantworten, die Funktion transformiert einen String mit Trennzeichen
zu einem Multi-Set mit 0 und 1. (Separate multiple choice)
}
\examples{
dat <-  data.frame(Q1=c(134,NA,35,134,5,24),
                   Q2=c(

                     "Alm Dudler, Essig, Cola",
                     NA,
                     "Cola, Holer",
                     "Alm Dudler, Cola, Essig",
                     "Holer",
                     "Bier, Essig"))
#dat


dat<- cbind(dat,separate_multiple_choice(dat$Q2))

dat<- cbind(dat,separate_multiple_choice(dat$Q1,
                                         label=c(
                                           "Alm Dudler","Bier", "Cola", "Essig", "Holer") ))
names(dat)<- GetLabelOrName(dat)
dat
x<-data.frame(A=c(15911,261011,3711,4811))
separate_multiple_choice(x$A)
}
