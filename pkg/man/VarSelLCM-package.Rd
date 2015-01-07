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
The main functions to use is \code{\link{VarSelCluster}}. It carries out the model selection by maximizing the MICL criterion.
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
data(banknote)

results <- VarSelCluster(banknote[,-1], 2, parallel=FALSE)

summary(results)
print(results)

\dontrun{
require(multtest)
data(golub)

results <- VarSelCluster(t(golub), 2)

summary(results)

}

}

\keyword{ package }
