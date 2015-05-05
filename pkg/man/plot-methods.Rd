\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,VSLCMresultsContinuous-method}
\alias{plot,VSLCMresultsInteger-method}
\alias{plot,VSLCMresultsCategorical-method}
\alias{plot,VSLCMresultsMixed-method}
\title{Produce result plot of  results provided by function VarSelCluster}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresultsContinuous}}, \code{\linkS4class{VSLCMresultsInteger}}, \code{\linkS4class{VSLCMresultsCategorical}}  or \code{\linkS4class{VSLCMresultsMixed}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce result plot of a class VSLCMresultsContinuous, VSLCMresultsInteger, VSLCMresultsCategorical or VSLCMresultsMixed.
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
res_with <- VarSelCluster(x, 2, nbcores = 2, initModel=30)

# Parameters of the best model
plot(res_with)
}
\seealso{
  \code{\link{plot}}
}
