\name{VarSelImputation}
\alias{VarSelImputation}
\title{VarSelImputation}

\description{
This function performs the imputation on individuals having missing values.
It uses the parameters resulting form function \link{VarSelCluster}.
Imputation is made by taking the expectation of the missing variables conditionally on 
the observed variables and on the parameters.
}
\usage{
VarSelImputation(obj, ind)
}

\arguments{
  \item{obj}{An object resulting from function \link{VarSelCluster} (i.e. an object of
  VSLCMresultsContinuous, VSLCMresultsInteger, VSLCMresultsCategorical or VSLCMresultsMixed.}
  
  \item{ind}{
    An vector of indicating which individuals are imputated (by default, all the individuals having missing values are imputated).
  }
  
  
}
\value{
  An data.frame contaning the imputation made on the selected individuals.
}


\examples{
# Package loading
require(VarSelLCM)

# Data loading
data(hepatitis)

# The class membership is deleted
x <- hepatitis[,-ncol(hepatitis)]

# Variable selection and parameter estiamtion
res_with <- VarSelCluster(x, 2, nbcores = 2)

# Imputation of the individuals : 119, 142, 148 and 149
ximput <- VarSelImputation(res_with, c(119,142, 148, 149))

# Individuals without imputation
print(x[c(119,142,148,149),])

# Individuals with imputation
print(ximput)
}
