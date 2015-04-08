########################################################################################################################
## Surcharge de la fonction print pour les objects de classe S4 VSLCMresultsContinuous et VSLCMresultsCategorical
########################################################################################################################

## Surcharge pour VSLCMresultsContinuous
setMethod(
  f="print",
  signature = c("VSLCMresultsContinuous"),
  definition = function(x){
    summary(x)    
    cat("\n Parameters per class:\n")
    for (k in 1:x@model@g){
      if (k>1){
        cat("*******************\n")
      }
      cat("Class",k,"\n")
      cat("Proportion:",x@param@pi[k],"\n")
      tmp <- data.frame(mean=x@param@mu[,k], sd=x@param@sd[,k])
      print(tmp)
      cat("\n")
    }
    
  }
)

## Surcharge pour VSLCMresultsContinuous
setMethod(
  f="print",
  signature = c("VSLCMresultsCategorical"),
  definition = function(x){
    summary(x)    
    cat("\n Parameters per class:\n")
    for (k in 1:x@model@g){
      if (k>1){
        cat("*******************\n")
      }
      cat("Class",k,"\n")
      cat("Proportion:",x@param@pi[k],"\n")
      for (j in 1:length(x@param@alpha)){
        cat(names(x@param@alpha)[j],"\n")
        print(x@param@alpha[[j]][k,])
        cat("\n")
      }
      cat("\n")
    }
    
  }
)

