\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,VSLCMresultsContinuous-method}
\alias{summary,VSLCMresultsCategorical-method}
\alias{summary,VSLCMresultsMixed-method}
\title{Produce result summary of  results provided by function VarSelCluster}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresultsCategorical}}, \code{\linkS4class{VSLCMresultsContinuous}} or \code{\linkS4class{VSLCMresultsMixed}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce result summary of a class VSLCMresultsCategorical, VSLCMresultsContinuous or VSLCMresultsMixed.
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
res_with <- VarSelCluster(x, 2, nbcores = 2)

# Parameters of the best model
summary(res_with)
}
\seealso{
  \code{\link{summary}}
}
