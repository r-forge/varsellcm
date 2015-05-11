\docType{methods}
\name{print}
\alias{print}
\alias{print,VSLCMresultsContinuous-method}
\alias{print,VSLCMresultsInteger-method}
\alias{print,VSLCMresultsCategorical-method}
\alias{print,VSLCMresultsMixed-method}
\title{Produce a presentation of results provided by function VarSelCluster}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresultsContinuous}}, \code{\linkS4class{VSLCMresultsInteger}}, \code{\linkS4class{VSLCMresultsCategorical}}  or \code{\linkS4class{VSLCMresultsMixed}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce a presentation of a  class VSLCMresultsContinuous, VSLCMresultsInteger, VSLCMresultsCategorical or VSLCMresultsMixed.
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

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 1, initModel=40)

# Parameters of the best model
print(res_with)
}
\seealso{
  \code{\link{print}}
}
