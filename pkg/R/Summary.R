########################################################################################################################
## Surcharge de la fonction summary 
########################################################################################################################
#'
#' Summary function.
#' 
#' This function gives the summary of an instance of  \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @name summary
#' @rdname summary-methods
#' @docType methods
#' @exportMethod summary
#' @aliases summary summary,VSLCMresults-method
setMethod(
  f="summary",
  signature = c("VSLCMresults"),
  definition = function(object){
    cat("Data set:\n   Number of individuals:", object@data@n,"\n")
    if (object@data@withContinuous){
      cat("   Number of continuous variables:", object@data@dataContinuous@d, "\n")
      val <- round(100*(1-mean(object@data@dataContinuous@notNA)),2)
      if (val>0)
        cat("   Percentile of missing values for the continuous variables:", val,"\n")
    }
    if (object@data@withInteger){
      cat("   Number of count variables:", object@data@dataInteger@d, "\n")
      val <- round(100*mean(1-object@data@dataInteger@notNA),2)
      if (val>0)
        cat("   Percentile of missing values for the integer variables:", val,"\n")
    }
    
    if (object@data@withCategorical){
      cat("   Number of categorical variables:", object@data@dataCategorical@d, "\n")
      miss <- 100*sum(sweep(is.na(object@data@dataCategorical@data),1,object@data@dataCategorical@weightdata,"*")) / (object@data@n * object@data@dataCategorical@d)
      if (miss>0)
        cat("   Percentile of missing values for the categorical variables:", miss,"\n")
    }
    cat("\n")
    
    cat("Model:\n   Number of components:", object@model@g, "\n   Number of relevant variables for the clustering:",sum(object@model@omega)," (", 100*sum(object@model@omega)/length(object@model@omega),"% ) \n")
    if (object@strategy@vbleSelec){
      cat("   The variable selection has been performed according to the", object@strategy@crit.varsel, " criterion \n")
    }else{
      cat("   No variable selection has been performed \n")
    }
    if (sum(object@model@omega)>0){
      if (length(length(object@model@names.relevant))>6){
        cat("   Names of the first six relevant variables for the clustering: ")        
      }else{
        cat("   Names of the relevant variables for the clustering: ") 
      }
      cat(object@model@names.relevant[1:min(6,length(object@model@names.relevant))], "\n\n")
    }     
    if ((length(object@criteria@degeneracyrate)==1)&&(object@criteria@degeneracyrate != 1)){
      cat("Information Criteria:\n")
      cat("   loglike:", object@criteria@loglikelihood,"\n")    
      cat("   AIC:    ", object@criteria@AIC,"\n")     
      cat("   BIC:    ", object@criteria@BIC,"\n")    
      cat("   ICL:    ", object@criteria@ICL,"\n")
      if ((object@strategy@crit.varsel=="MICL")&&(object@strategy@vbleSelec==TRUE)){        
        cat("   MICL:   ", object@criteria@MICL,"\n")    
        cat("   Best values has been found ", object@criteria@cvrate, "times\n")
      }  
    }
    cat("\n")
    if ((length(object@criteria@degeneracyrate)==1) && (object@criteria@degeneracyrate>0.1))
      cat("Warnings:\n  The rate of degeneracy for the EM algorithm is", object@criteria@degeneracyrate,"\n" )
  }
)