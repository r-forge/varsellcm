\docType{methods}
\name{print}
\alias{print}
\alias{print,VSLCMresults-method}
\title{Print an object of class VSLCMresults}
\arguments{
  \item{x}{An object of class \code{\linkS4class{VSLCMresults}}}
}
\value{
  NULL. 
}
\description{
 Print an object of class VSLCMresults
}
\examples{
require(VarSelLCM)
data(apples)
results <- VarSelCluster(apples, 2, parallel=FALSE)
print(results)
}
\seealso{
  \code{\link{print}}
}

