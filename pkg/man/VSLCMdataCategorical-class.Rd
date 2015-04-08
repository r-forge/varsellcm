\name{VSLCMdataCategorical-class}
\docType{class}
\alias{VSLCMdataCategorical-class}

\title{Class \code{"VSLCMdataCategorical"}}
\description{
This class contains the model information
}



\section{Slots}{
  \describe{
    \item{\code{n}:}{Number of individuals.}
    \item{\code{d}:}{Number of categorical variables.}
    \item{\code{data}:}{Whole data set.}
    \item{\code{shortdata}:}{Profiles.}
    \item{\code{weightdata}:}{Occurence of profiles}
    \item{\code{modalitynames}:}{List of the modalities for each variables.}
  }
}


\examples{
showClass("VSLCMdataCategorical")
}

\keyword{classes}
