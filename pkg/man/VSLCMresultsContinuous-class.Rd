\name{VSLCMresultsContinuous-class}
\docType{class}
\alias{VSLCMresultsContinuous-class}

\title{Class \code{"VSLCMresultsContinuous"}}
\description{
This class contains the model information
}



\section{Slots}{
  \describe{
    \item{\code{data}:}{A matrix, or data frame where rows correspond to observations and columns correspond to variables. }
    \item{\code{criteria}:}{An object of class \code{\linkS4class{VSLCMcriteria}}}
    \item{\code{partitions}:}{An object of class \code{\linkS4class{VSLCMpartitions}}}
    \item{\code{model}:}{An object of class \code{\linkS4class{VSLCMmodel}}}
    \item{\code{strategy}:}{An object of class \code{\linkS4class{VSLCMstrategy}}}
    \item{\code{param}:}{An object of class \code{\linkS4class{VSLCMparamContinuous}}}
  }
}


\examples{
showClass("VSLCMresultsContinuous")
}

\keyword{classes}
