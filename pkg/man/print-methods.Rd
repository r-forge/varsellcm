\docType{methods}
\name{print}
\alias{print}
\alias{print,VSLCMresultsContinuous-method}
\alias{print,VSLCMresultsCategorical-method}
\title{Produce a presentation of a VSLCMresultsCategorical or VSLCMresultsContinuous class}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresultsCategorical}} or \code{\linkS4class{VSLCMresultsContinuous}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce a presentation of a VSLCMresultsCategorical or VSLCMresultsContinuous class
}
\examples{
require(VarSelLCM)
data(stones)

results <- VarSelCluster(stones, 2)

print(results)

}
\seealso{
  \code{\link{print}}
}
