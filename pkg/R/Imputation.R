ImputCont <- function(data, tik, param){
  output <- as.data.frame(data@data)
  for (j in 1:data@d){
    if (sum(data@notNA[,j])<data@n){
      who <- which(data@notNA[,j]==0)
      output[who, j] <- 0
      for (k in 1:ncol(tik))
        output[who, j] <- output[who, j] + tik[who,k]*param@mu[j,k]
    }  
  }
  return(output)
}


ImputCate <- function(data, tik, param){
  output <- as.data.frame(data@data)
  for (j in 1:data@d){
    if (any(is.na(data@data[,j]))){
      who <- which(is.na(data@data[,j])==T)
      output[who, j] <- apply(tik[who,]%*%param@alpha[[j]],1,which.max)
    }
    output[,j] <- as.factor(data@modalitynames[[j]][output[,j]])
  }
  colnames(output) <- colnames(data@shortdata)
  return(output)
}

VarSelImputation <- function(obj, ind){
  if (missing(ind))
    ind <- 1:obj@data@n
  
  if (any( (ind %in% 1:obj@data@n)==FALSE))
    stop("Indice of individual is not correct!")
  
  output <- NULL
  
  if (class(obj)=="VSLCMresultsContinuous")
    output <- ImputCont(obj@data, obj@partitions@tik, obj@param)[ind,]
  else if (class(obj)=="VSLCMresultsCategorical")
    output <- ImputCate(obj@data, obj@partitions@tik, obj@param)[ind,]
  else if (class(obj)=="VSLCMresultsMixed")
    output <- cbind(ImputCont(obj@data@dataContinuous, obj@partitions@tik, obj@param@paramContinuous)[ind,], ImputCate(obj@data@dataCategorical, obj@partitions@tik, obj@param@paramCategorical)[ind,])
  else
    stop("obj don't arise from function VarSelCluster")
  
  
  return(output)         
}

