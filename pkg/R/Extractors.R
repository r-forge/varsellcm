

########################################################################################################################
## BIC extractor
########################################################################################################################
#'
#' BIC criterion.
#' 
#' This function gives the BIC criterion (according to the formula log-likelihood - log(n)*npar/2, where npar represents the number of parameters in the fitted model and n represents the sample size)  for an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name BIC
#' @rdname BIC-methods
#' @docType methods
#' @exportMethod BIC
#' @aliases BIC BIC,VSLCMresults-method
#' @references Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), 461–464.

setMethod(f="BIC",
          signature = c("VSLCMresults"),
          definition = function(object) object@criteria@BIC)

########################################################################################################################
## AIC extractor
########################################################################################################################
#'
#' AIC criterion.
#' 
#' This function gives the AIC criterion (according to the formula log-likelihood - npar, where npar represents the number of parameters in the fitted model)  for an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name AIC
#' @rdname AIC-methods
#' @docType methods
#' @exportMethod AIC
#' @aliases AIC AIC,VSLCMresults-method
#' @references Akaike, H. (1974), "A new look at the statistical model identification", IEEE Transactions on Automatic Control, 19 (6): 716–723.

setMethod(f="AIC",
          signature = c("VSLCMresults"),
          definition = function(object) object@criteria@AIC)

###################################################################################
##' MICL
##'
##' @description  
##' This function gives the MICL criterion for an instance of \code{\linkS4class{VSLCMresults}}.
##' 
##' @param object \code{\linkS4class{VSLCMresults}}
##' 
##' @references Marbac, M. and Sedki, M. (2017). Variable selection for model-based clustering using the integrated completed-data likelihood. Statistics and Computing, 27 (4), 1049-1063.
##' 
##' 
##' @examples
##' \dontrun{
##' data("heart")
##' z <- heart[,"Class"]
##' x <- heart[,-13]
##' object <- VarSelCluster(x, 2, vbleSelec = TRUE, crit.varsel = "MICL")
##' MICL(object)
##' }
##' @export
##'
##'
MICL <- function(object){
  check.results(object)
  if (length(object@criteria@MICL)==0) stop("This criterion wasn't computed during the model selection")
  object@criteria@MICL
}

###################################################################################
##' ICL
##'
##' @description  
##' This function gives the ICL criterion for an instance of \code{\linkS4class{VSLCMresults}}.
##' 
##' @param object \code{\linkS4class{VSLCMresults}}
##' 
##' @references Biernacki, C., Celeux, G., and Govaert, G. (2000). Assessing a mixture model for clustering with the integrated completed likelihood. IEEE transactions on pattern analysis and machine intelligence, 22(7), 719–725.
##' 
##' 
##' @examples
##' \dontrun{
##' data("heart")
##' z <- heart[,"Class"]
##' x <- heart[,-13]
##' object <- VarSelCluster(x, 2, vbleSelec = FALSE)
##' ICL(object)
##' }
##' @export
##'
##'
ICL <- function(object){
  check.results(object)
  object@criteria@ICL
}

########################################################################################################################
## fitted
########################################################################################################################
#'
#' Extract the estimated partition
#' 
#' This function returns the partition among the observations of an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name fitted
#' @rdname fitted-methods
#' @docType methods
#' @exportMethod fitted
#' @aliases fitted fitted,VSLCMresults-method

setMethod(f="fitted",
          signature = c("VSLCMresults"),
          definition = function(object) object@partitions@zMAP)

########################################################################################################################
## fitted
########################################################################################################################
#'
#' Extract the estimated partition
#' 
#' This function returns the partition among the observations of an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name fitted
#' @rdname fitted-methods
#' @docType methods
#' @exportMethod fitted
#' @aliases fitted fitted,VSLCMresults-method

setMethod(f="fitted",
          signature = c("VSLCMresults"),
          definition = function(object) object@partitions@zMAP)

########################################################################################################################
## fitted.values
########################################################################################################################
#'
#' Extract the estimated partition
#' 
#' This function returns the partition among the observations of an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name fitted
#' @rdname fitted-methods
#' @docType methods
#' @exportMethod fitted.values
#' @aliases fitted.values fitted.values,VSLCMresults-method

setMethod(f="fitted.values",
          signature = c("VSLCMresults"),
          definition = function(object) object@partitions@zMAP)


########################################################################################################################
## coef
########################################################################################################################
#'
#' Extract the parameters
#' 
#' This function returns an instance of class \code{\linkS4class{VSLCMparam}} which contains the model parameters.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name coef
#' @rdname coef-methods
#' @docType methods
#' @exportMethod coef
#' @aliases coef coef,VSLCMresults-method

setMethod(f="coef",
          signature = c("VSLCMresults"),
          definition = function(object) object@param)

########################################################################################################################
## coefficients
########################################################################################################################
#'
#' Extract the parameters
#' 
#' This function returns an instance of class \code{\linkS4class{VSLCMparam}} which contains the model parameters.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name coefficients
#' @rdname coefficients-methods
#' @docType methods
#' @exportMethod coefficients
#' @aliases coefficients coefficients,VSLCMresults-method

setMethod(f="coefficients",
          signature = c("VSLCMresults"),
          definition = function(object) object@param)
