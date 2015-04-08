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
require(VarSelLCM)
data(stones)

results <- VarSelCluster(stones, 2)

summary(results)

}
\seealso{
  \code{\link{summary}}
}
