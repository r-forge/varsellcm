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
# Package loading
require(VarSelLCM)

# Data loading:
# x contains the observed variables
# z the known statu (i.e. 1: absence and 2: presence of heart disease)
data(heart)
z <- heart[,"Class"]
x <- heart[,-13]

# Cluster analysis without variable selection
res_without <- VarSelCluster(x, 2, vbleSelec = F)

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 2)

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