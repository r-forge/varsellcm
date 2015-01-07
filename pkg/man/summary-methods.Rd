\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,VSLCMresults-method}
\title{Produce result summary of a VSLCMresults class}
\arguments{
  \item{object}{An object of class \code{\linkS4class{VSLCMresults}}}
}
\value{
  NULL. Summaries to standard out.
}
\description{
  Produce result summary of a VSLCMresults class
}
\examples{
require(VarSelLCM)
data(apples)
results <- VarSelCluster(apples, 2)
summary(results)
}
\seealso{
  \code{\link{summary}}
}

