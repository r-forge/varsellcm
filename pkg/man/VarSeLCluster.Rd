\name{VarSelCluster}
\alias{VarSelCluster}
\title{
VarSelCluster
}

\description{
   This function performs the variable selection for a model-based clustering of data set having missing values. 
   The data distribution follow a locally independent mixture model whose the one-dimensional marginal distributions
   belong to the exponential family (i.e. Gaussian or multinomial).
   Missing values are dealing by assuming that data are missing at random.
   The model selection is carried out by maximizing the MICL criterion.
}
\usage{
VarSelCluster(x, g, initModel=50, vbleSelec=TRUE, paramEstim=TRUE, parallel=FALSE, nbSmall=250, iterSmall=20, nbKeep=50, iterKeep=10**3, tolKeep=10**(-3))
}

\arguments{
  \item{x}{
    A data frame where rows correspond to observations and columns correspond to variables. Continuous variables must be  "numeric"  and categorical variables must be "factor".

  }
  \item{g}{
    An integer specifying the number of mixture components
  }
  \item{initModel}{
    The number of random initializations of the algorithm. 
    The default is 50. 
  }
  \item{vbleSelec}{
    Logical indicating if a variable selection is performed. 
    The default is TRUE. 
  }
  \item{paramEstim}{
    Logical indicating if the maximum likelihood (ML) inference is performed for the best model (according to the MICL criterion). 
    The default is TRUE. 
  }
  \item{parallel}{
    Logical indicating if the code is parallelized or not.
    The default is FALSE.
  }
  \item{nbSmall}{
    Numeric indicating the number of SmallEM algorithms performed for the ML inference. 
    The default is 250. 
  }
  \item{iterSmall}{
    Numeric indicating the number of iterations for each SmallEM algorithm.
    The default is 20. 
  }
  \item{nbKeep}{
    Numeric indicating the number of chains used for the final EM algorithm.
    The default is 50. 
  }
  \item{iterKeep}{
    Numeric indicating the maximal number of iterations for each EM algorithm.
    The default is 1000. 
  }
  \item{tolKeep}{
    Numeric indicating the maximal gap between two successive iterations of EM algorithm which stops the algorithm.
    The default is 0.001. 
  }

}
\value{
  An object of class \code{\linkS4class{VSLCMresultsContinuous}} or \code{\linkS4class{VSLCMresultsCategorical}} providing the model maximizing the MICL criterion.
  Thus, it indicates which variables are relevant to the clustering. It also provides the maximum
  likelihood estimates associated to this model.
}

\references{
  M. Marbac and M. Sedki (2015).
  Variable selection for model-based clustering using the integrated completed-data likelihood.
  \emph{Preprint}. 
}


\examples{
# Variable selection and parameter estimation of a continuous data set with missing values
require(VarSelLCM)
data(stones)

results <- VarSelCluster(stones, 2)

summary(results)

print(results)

# Variable selection and parameter estimation of a categorical data set without missing values
data(birds)

results <- VarSelCluster(birds, 2)

summary(results)

print(results)
}
