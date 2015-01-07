\name{VarSelLCM-package}
\alias{VarSelLCM-package}
\alias{VarSelLCM}
\docType{package}


\title{
Variable selection for model-based clustering using the integrated completed-data likelihood of a Latent Class Model
}

\description{
Variable Selection for model-based clustering by using a mixture model of Gaussian distribution assuming conditional independence between variables. The algorithm carries out the model selection by optimizing the MICL criterion which has a closed form for such a distribution.
}

\details{
\tabular{ll}{
Package: \tab VarSelLCM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-25\cr
License: \tab GPL (>= 2)\cr
}
Main functions to use:
\tabular{ll}{
  Model Selection by the MICL criterion: \tab \code{\link{VarSelCluster}} \cr
  MICL computation for known model: \tab \code{\link{VarSelModelKnown}} 
  }
}

\author{
Matthieu Marbac and Mohammed Sedki

Maintainer: Matthieu Marbac <matthieu.marbac@inserm.fr> and Mohammed Sedki <mohammed.sedki@inserm.fr>
}

\references{
  M. Marbac and M. Sedki (2015).
  Variable selection for model-based clustering using the integrated completed-data likelihood.
  \emph{Preprint}. 
}


\examples{
# Variable selection
require(VarSelLCM)
data(apples)
results <- VarSelCluster(apples, 2)

}

\keyword{ package }
