########################################################################################################################
## Surcharge de la fonction print pour les objects de classe S4 VSLCMresultsContinuous et VSLCMresultsCategorical
########################################################################################################################

## Surcharge pour VSLCMresultsContinuous
setMethod(
  f="print",
  signature = c("VSLCMresultsContinuous"),
  definition = function(x){
    summary(x)    
    if (x@criteria@degeneracyrate != 1){
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
  }
)

## Surcharge pour VSLCMresultsInteger
setMethod(
  f="print",
  signature = c("VSLCMresultsInteger"),
  definition = function(x){
    summary(x)    
    if (x@criteria@degeneracyrate != 1){
      cat("\n Parameters per class:\n")
      for (k in 1:x@model@g){
        if (k>1){
          cat("*******************\n")
        }
        cat("Class",k,"\n")
        cat("Proportion:",x@param@pi[k],"\n")
        tmp <- data.frame(lambda=x@param@lambda[,k])
        print(tmp)
        cat("\n")
      }
    }
  }
)

## Surcharge pour VSLCMresultsCategorical
# setMethod(
#   f="print",
#   signature = c("VSLCMresultsCategorical"),
#   definition = function(x){
#     summary(x)    
#     cat("\n Parameters per class:\n")
#     for (k in 1:x@model@g){
#       if (k>1){
#         cat("*******************\n")
#       }
#       cat("Class",k,"\n")
#       cat("Proportion:",x@param@pi[k],"\n")
#       for (j in 1:length(x@param@alpha)){
#         cat(names(x@param@alpha)[j],"\n")
#         print(x@param@alpha[[j]][k,])
#         cat("\n")
#       }
#       cat("\n")
#     }
#     
#   }
# )
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
      maxcol <- 0
      for (j in 1:x@data@d) maxcol <- max(maxcol, length(x@data@modalitynames[[j]]))
      alpha <- matrix(0,x@data@d, maxcol)
      for (j in 1:x@data@d)  alpha[j,1:length(x@data@modalitynames[[j]])] <- round(x@param@alpha[[j]][k,],6)
      for (j in 1:ncol(alpha)) alpha[,j] <- as.character(alpha[,j])
      for (j in 1:x@data@d){
        if (length(x@data@modalitynames[[j]])<maxcol)
          alpha[j, (length(x@data@modalitynames[[j]])+1):maxcol] <- rep(".", maxcol-length(x@data@modalitynames[[j]]))
        
      }
      alpha <- data.frame(alpha)
      colnames(alpha) <- paste("Level",1:ncol(alpha),sep=".")
      rownames(alpha) <- names(x@param@alpha)
      print(alpha)
      cat("\n")
    }
    
  }
)


## Surcharge pour VSLCMresultsContinuous
setMethod(
  f="print",
  signature = c("VSLCMresultsMixed"),
  definition = function(x){
    summary(x)    
    if (x@criteria@degeneracyrate != 1){
      cat("\n Parameters per class:\n")
      for (k in 1:x@model@g){
        if (k>1){
          cat("*******************\n")
        }
        cat("Class",k,"\n")
        cat("Proportion:",x@param@pi[k],"\n")
        if (x@data@withContinuous){
          cat("Parameters of continuous variable \n")
          tmp <- data.frame(mean=x@param@paramContinuous@mu[,k], sd=x@param@paramContinuous@sd[,k])
          print(tmp)
          cat("\n")
        }
        if (x@data@withInteger){
          cat("Parameters of integer variable \n")
          tmp <- data.frame(lambda=x@param@paramInteger@lambda[,k])
          rownames(tmp)  <- rownames(x@param@paramInteger@lambda)
          print(tmp)
          cat("\n")
        }
        
        if (x@data@withCategorical){
          cat("Parameters of categorical variables \n")      
          maxcol <- 0
          for (j in 1:x@data@dataCategorical@d) maxcol <- max(maxcol, length(x@data@dataCategorical@modalitynames[[j]]))
          alpha <- matrix(0,x@data@dataCategorical@d, maxcol)
          for (j in 1:x@data@dataCategorical@d)  alpha[j,1:length(x@data@dataCategorical@modalitynames[[j]])] <- round(x@param@paramCategorical@alpha[[j]][k,],6)
          for (j in 1:ncol(alpha)) alpha[,j] <- as.character(alpha[,j])
          for (j in 1:x@data@dataCategorical@d){
            if (length(x@data@dataCategorical@modalitynames[[j]])<maxcol)
              alpha[j, (length(x@data@dataCategorical@modalitynames[[j]])+1):maxcol] <- rep(".", maxcol-length(x@data@dataCategorical@modalitynames[[j]]))
            
          }
          alpha <- data.frame(alpha)
          colnames(alpha) <- paste("Level",1:ncol(alpha),sep=".")
          rownames(alpha) <- names(x@param@paramCategorical@alpha)
          print(alpha)
          cat("\n")
        }
        
        cat("\n")
      }
    }
  }
)


# 
# ## Surcharge pour VSLCMresultsContinuous
# setMethod(
#   f="print",
#   signature = c("VSLCMresultsMixed"),
#   definition = function(x){
#     summary(x)    
#     if (x@criteria@degeneracyrate != 1){
#       cat("\n Parameters per class:\n")
#       for (k in 1:x@model@g){
#         if (k>1){
#           cat("*******************\n")
#         }
#         cat("Class",k,"\n")
#         cat("Proportion:",x@param@pi[k],"\n")
#         if (x@data@withContinuous){
#           cat("Parameters of continuous variable \n")
#           tmp <- data.frame(mean=x@param@paramContinuous@mu[,k], sd=x@param@paramContinuous@sd[,k])
#           print(tmp)
#           cat("\n")
#         }
#         if (x@data@withInteger){
#           cat("Parameters of integer variable \n")
#           tmp <- data.frame(lambda=x@param@paramInteger@lambda[,k])
#           rownames(tmp)  <- rownames(x@param@paramInteger@lambda)
#           print(tmp)
#           cat("\n")
#         }
#         
#         if (x@data@withCategorical){
#           cat("Parameters of categorical variables \n")      
#           for (j in 1:length(x@param@paramCategorical@alpha)){
#             cat(names(x@param@paramCategorical@alpha)[j],"\n")
#             print(x@param@paramCategorical@alpha[[j]][k,])
#             cat("\n")
#           }
#         }
#         
#         cat("\n")
#       }
#     }
#   }
# )

