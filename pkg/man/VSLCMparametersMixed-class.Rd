\name{VSLCMparamMixed-class}
\docType{class}
\alias{VSLCMparamMixed-class}

\title{Class \code{"VSLCMparamMixed"}}
\description{
  This class contains the model information
}
\section{Slots}{
  \describe{
    \item{\code{pi}:}{proportions.}
    \item{\code{paramContinuous}:}{Instance of \code{\linkS4class{VSLCMparamContinuous}}  related to the continuous variables.}
    \item{\code{paramInteger}:}{Instance of \code{\linkS4class{VSLCMparamInteger}}  related to the integer variables.}
    \item{\code{paramCategorical}:}{Instance of \code{\linkS4class{VSLCMparamCategorical}}  related to the categorical variables.}
  }
}
\examples{
  showClass("VSLCMparamMixed")
}
\keyword{classes}
