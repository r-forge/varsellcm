\name{VarSelCluster}
\alias{VarSelCluster}
\title{VarSelCluster}

\description{
  Variable Selection for model-based clustering managed by the Latent Class Model. This model analyses mixed-type data (data with continuous and/or integer and/or categorical variables) with missing values (missing at random) by assuming independence between classes. The one-dimensional marginals of the components follow standard distributions for facilitating both the model interpretation and the model selection. The variable selection is led by an alternated optimization procedure for maximizing the MICL criterion. The maximum likelihood inference is done by an EM algorithm for the selected model.
  }
\usage{
VarSelCluster(x, g, initModel=50, vbleSelec=TRUE, 
paramEstim=TRUE, nbcores=1, nbSmall=250, iterSmall=20,
nbKeep=50, iterKeep=10**3, tolKeep=10**(-3))
}

\arguments{
  \item{x}{A data frame where rows correspond to observations and columns correspond to variables. Continuous variables must be  "numeric"  and categorical variables must be "factor".}
  
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
  
  \item{nbcores}{
    Number of cores.
    The default is 1.
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
  An object of class \code{\linkS4class{VSLCMresultsContinuous}}, \code{\linkS4class{VSLCMresultsCategorical}} or \code{\linkS4class{VSLCMresultsMixed}}  containing the model maximizing the MICL criterion.
  Thus, it indicates which variables are relevant to the clustering. It also provides the maximum likelihood estimates associated to this model.
}

\references{
  M. Marbac and M. Sedki (2015).
  Variable selection for model-based clustering using the integrated completed-data likelihood.
  \emph{Preprint}. 
}


\examples{
# Package loading
require(Rcpp)
require(RcppArmadillo)
require(VarSelLCM)

# Data loading:
# x contains the observed variables
# z the known statu (i.e. 1: absence and 2: presence of heart disease)
data(heart)
z <- heart[,"Class"]
x <- heart[,-13]

# Cluster analysis without variable selection
res_without <- VarSelCluster(x, 2, vbleSelec = FALSE)

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 2, initModel=40)

# Confusion matrices: variable selection decreases the misclassification error rate
print(table(z, res_without@partitions@zMAP))
print(table(z, res_with@partitions@zMAP))

# Summary of the best model
summary(res_with)

# Parameters of the best model
print(res_with)

# Plot of the best model
plot(res_with)
}
