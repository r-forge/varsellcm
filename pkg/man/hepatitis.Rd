\docType{data}
\name{hepatitis}
\alias{hepatitis}
\title{Mixed data with missing values}
\format{A data frame with 155 observations on the following 19 variables and grouped into two classes.

\describe{

  \item{\code{Bilirubin}}{continuous}
  \item{\code{AlbuMin}}{continuous}
  \item{\code{Age}}{integer}
  \item{\code{AlkPhosphate}}{integer}
  \item{\code{Sgot}}{integer}
  \item{\code{ProTime}}{integer}
  \item{\code{Sex}}{binary}
  \item{\code{Steroid}}{binary}
  \item{\code{Antivirals}}{binary}
  \item{\code{Fatigue}}{binary}
  \item{\code{Malaise}}{binary}
  \item{\code{Anorexia }}{binary}
  \item{\code{LiverBig}}{binary}
  \item{\code{LiverFirm}}{binary}
  \item{\code{SpleenPalpable}}{binary}
  \item{\code{Spiders}}{binary}
  \item{\code{Ascites}}{binary}
  \item{\code{Varices}}{binary}
  \item{\code{Histology}}{binary}
  \item{\code{Class}}{binary: 1-die, 2-survive}
 }}
\source{
Diaconis,P. & Efron,B. (1983). Computer-Intensive Methods in Statistics. Scientific American, Volume 248. 
}
\description{
This data set contains a mixture of integer and real valued attributes, with information about patients affected by the Hepatitis disease.

The task is to predict if these patients will die (1) or survive (2).
}
\examples{
data(hepatitis)
}
\keyword{datasets}

