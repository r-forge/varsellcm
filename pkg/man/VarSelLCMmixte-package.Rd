\name{VarSelLCMmixte-package}
\alias{VarSelLCMmixte-package}
\alias{VarSelLCMmixte}
\docType{package}


\title{
Variable Selection for Model-Based Clustering of continuous, categorical or mixed data set with missing values.
}

\description{
This package performs the variable selection for the cluster analysis of a continuous, categorical or mixed data set having potentially missing values. The cluster analysis is achieved by a mixture model assuming the conditional independence between variables whose the one-dimensional marginal distributions belong to the exponential family (i.e. Gaussian or multinomial distribution). The variable selection is achieved by optimizing the MICL criterion which has a closed 
form for such a mixture model. Finally, the maximum likelihood estimation is performed by an EM algorithm which can be also used without selecting variables.
}

\details{
\tabular{ll}{
Package: \tab VarSelLCMmixte\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2014-11-25\cr
License: \tab GPL (>= 2)\cr
}
The main function to use is \code{\link{VarSelCluster}}. It carries out the model selection by maximizing the MICL criterion and it performs the maximum likelihood estimation of the selected model.
}

\author{
Matthieu Marbac and Mohammed Sedki
Maintainer: Mohammed Sedki <mohammed.sedki@u-psud.fr>
}

\references{
  M. Marbac and M. Sedki (2015).
  Variable selection for model-based clustering using the integrated completed-data likelihood.
  \emph{Preprint}. 
}


\examples{
# Variable selection and parameter estimation of a continuous data set with missing values
require(VarSelLCM)
data(stones)

results <- VarSelCluster(stones, 2)

summary(results)

print(results)

# Variable selection and parameter estimation of a categorical data set without missing values
data(birds)

results <- VarSelCluster(birds, 2)

summary(results)

print(results)

\dontrun{
## To install multitest package : uncomment the following lines
## source("http://bioconductor.org/biocLite.R")
## biocLite("multtest")
require(VarSelLCM)
require(multtest)
data(golub)

results <- VarSelCluster(t(golub), 2)

summary(results)

}

}

\keyword{ package }
