\name{VarSelLCM-package}
\alias{VarSelLCM-package}
\alias{VarSelLCM}
\docType{package}


\title{
 Variable Selection in model-based clustering managed by the Latent Class Model for analysis mixed-type data with missing values.
}

\description{
The package uses a finite mixture model for analyzing mixed-type data (data with continuous and/or integer and/or categorical variables) with missing values (missing at random) by assuming independence between classes. The one-dimensional marginals of the components follow standard distributions for facilitating both the model interpretation and the model selection. The variable selection is led by an alternated optimization procedure for maximizing the MICL criterion. The maximum likelihood inference is done by an EM algorithm for the selected model. This package also performs the imputation of  missing values.
}

\details{
\tabular{ll}{
Package: \tab VarSelLCM\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2015-06-01\cr
License: \tab GPL (>= 2)\cr
}
The main functions to use are \code{\link{VarSelCluster}} and \code{\link{VarSelImputation}}.

Function \code{\link{VarSelCluster}} carries out the model selection by maximizing the MICL criterion, then it performs the maximum likelihood estimation of the selected model via an EM algorithm.

Function \code{\link{VarSelImputation}} performs the imputation of  missing values by taking the expectation of the missing values conditionally on the model, its parameters and on the observed variables.

Tool methods \code{summary}, \code{print} and \code{plot} are available for facilitating the interpretation.
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
# Package loading
# Package loading
require(Rcpp)
require(RcppArmadillo)
require(VarSelLCM)

# Data loading:
# x contains the observed variables
# z the known statu (i.e. 1: absence and 2: presence of heart disease)
data(heart)
z <- heart[,"Class"]
x <- heart[,-13]

# Cluster analysis without variable selection
res_without <- VarSelCluster(x, 2, vbleSelec = FALSE)

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 2, initModel=40)

# Confusion matrices: variable selection decreases the misclassification error rate
print(table(z, res_without@partitions@zMAP))
print(table(z, res_with@partitions@zMAP))

# Summary of the best model
summary(res_with)

# Parameters of the best model
print(res_with)

# Plot of the best model
plot(res_with)
}
\keyword{ package }