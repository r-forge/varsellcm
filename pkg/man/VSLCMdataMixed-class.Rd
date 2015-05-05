\name{VSLCMdataMixed-class}
\docType{class}
\alias{VSLCMdataMixed-class}

\title{Class \code{"VSLCMdataMixed"}}
\description{
  This class contains the data information
}

\section{Slots}{
  \describe{
    \item{\code{n}:}{Number of individuals.}
    \item{\code{d}:}{Number of Mixed variables.}
    \item{\code{withContinuous}:}{Logical indicating if there is at least one continuous variable.}
    \item{\code{withInteger}:}{Logical indicating if there is at least one integer variable.}
    \item{\code{withCategorical}:}{Logical indicating if there is at least one categorical variable.}
    \item{\code{dataContinuous}:}{Instance of \code{\linkS4class{VSLCMdataContinuous}}  related to the continuous variables.}
    \item{\code{dataInteger}:}{Instance of \code{\linkS4class{VSLCMdataInteger}}  related to the integer variables.}
    \item{\code{dataCategorical}:}{Instance of \code{\linkS4class{VSLCMdataCategorical}}  related to the categorical variables.}
  }
}
\examples{
  showClass("VSLCMdataMixed")
}
\keyword{classes}