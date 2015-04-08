\name{VSLCMdataContinuous-class}
\docType{class}
\alias{VSLCMdataContinuous-class}

\title{Class \code{"VSLCMdataContinuous"}}
\description{
This class contains the model information
}



\section{Slots}{
  \describe{
    \item{\code{n}:}{Number of individuals.}
    \item{\code{d}:}{Number of continuous variables.}
    \item{\code{data}:}{Whole data set.}
    \item{\code{notNA}:}{Logical indicating if the realization is missing.}
    \item{\code{priors}:}{values of the hper-parameters.}
  }
}


\examples{
showClass("VSLCMdataContinuous")
}

\keyword{classes}
