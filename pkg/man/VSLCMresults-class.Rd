\name{VSLCMresults-class}
\docType{class}
\alias{VSLCMresults-class}

\title{Class \code{"VSLCMresults"}}
\description{
This class contains the model informations
}



\section{Slots}{
  \describe{
    \item{\code{data}:}{A matrix, or data frame where rows correspond to observations and columns correspond to variables. }
    \item{\code{priors}:}{Matrix where rows indicate the values of the hyper-parameters}
    \item{\code{criteria}:}{An object of class \code{\link{VSLCMcriteria}}}
    \item{\code{partitions}:}{An object of class \code{\link{VSLCMpartitions}}}
    \item{\code{model}:}{An object of class \code{\link{VSLCMmodel}}}
    \item{\code{parameters}:}{An object of class \code{\link{VSLCMparameters}}}
  }
}


\examples{
showClass("VSLCMresults")
}

\keyword{classes}
