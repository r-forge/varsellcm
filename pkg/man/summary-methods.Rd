\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,VSLCMresultsContinuous-method}
\alias{summary,VSLCMresultsCategorical-method}
\title{Produce result summary of a VSLCMresultsCategorical or VSLCMresultsContinuous class}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresultsCategorical}} or \code{\linkS4class{VSLCMresultsContinuous}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce result summary of a VSLCMresultsCategorical or VSLCMresultsContinuous classs
}
\examples{
# Package loading
require(VarSelLCM)

# Data loading:
# x contains the observed variables
# z the known statu (i.e. 1: absence and 2: presence of heart disease)
data(heart)

res_without <- VarSelCluster(x, 2, vbleSelec = F)


# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 2)



# Parameters of the best model
summary(res_with)

}
\seealso{
  \code{\link{summary}}
}
