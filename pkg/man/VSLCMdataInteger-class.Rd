\name{VSLCMdataInteger-class}
\docType{class}
\alias{VSLCMdataInteger-class}

\title{Class \code{"VSLCMdataInteger"}}
\description{
This class contains the data information
}

\section{Slots}{
  \describe{
    \item{\code{n}:}{Number of individuals.}
    \item{\code{d}:}{Number of Integer variables.}
    \item{\code{data}:}{Whole data set.}
    \item{\code{notNA}:}{Logical indicating if the realization is missing.}
    \item{\code{priors}:}{Values of the hyper-parameters. One row corresponds to one variable, and hyperparameters are set as follows: alpha=beta=1.}
  }
}


\examples{
showClass("VSLCMdataInteger")
}

\keyword{classes}
