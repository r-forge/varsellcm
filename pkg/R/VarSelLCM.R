VarSelModelSelection <- function(x, g, nbinit=30,  parallel=TRUE){
  if (parallel == FALSE){
    
    reference <- new("VSLCMresults", criteria = new("VSLCMcriteria", likelihood=-Inf, BIC=-Inf, ICL=-Inf, MICL=-Inf))
    for (it in 1:nbinit){
      where <- VarSelStartingPoint(x, g)
      cand <- OptimizeMICL(where, 1)
      if (cand@criteria@MICL > reference@criteria@MICL)
        reference <- cand
    }
    
  }else{
    
    reference <- list()
    for (it in 1:nbinit)
      reference[[it]] <- VarSelStartingPoint(x, g)
    
    nb.cpus <- min(detectCores(all.tests = FALSE, logical = FALSE) , nbinit)
    if(Sys.info()["sysname"] == "Windows")
    {
      cl <- makeCluster(nb.cpus)
      common.objects <- c("reference")
      clusterExport(cl=cl, varlist = common.objects, envir = environment())
      reference <- parLapply(cl = cl, 
                             X  = reference, 
                             fun = OptimizeMICL)
      stopCluster(cl)
      
    }
    else
      reference <- mclapply(X = reference,
                            FUN = OptimizeMICL,
                            optimize=1,
                            mc.cores = nb.cpus,
                            mc.preschedule = TRUE,
                            mc.cleanup = TRUE
      )
    tmp <- rep(0, nbinit)
    for (it in 1:nbinit){
      tmp[it] <- reference[[it]]@criteria@MICL
    }
    
    
    
    
    reference <- reference[[which(tmp == max(tmp))[1]]]   
    
    
    
    
  }
  
  return( reference )
}

# VarSelParamEstim <- function(obj){
#   
#   obj@criteria@likelihood <- 0
#   obj@criteria@BIC <- 0
#   obj@criteria@ICL <- 0
#   
#   obj@parameters@means <- matrix(0, obj@model@g, ncol(obj@data))
#   rownames(obj@parameters@means ) <- paste("Class", 1:obj@model@g)
#   obj@parameters@variances <- matrix(0, obj@model@g, ncol(obj@data))
#   rownames(obj@parameters@variances ) <- paste("Class", 1:obj@model@g)
#   
#   obj@partitions@zOPT <- as.numeric(obj@partitions@zOPT) + 1
#   
#   
#   if (sum(obj@model@omega) > 0){
#     discrim <- mixmodCluster(data = as.data.frame(obj@data[, which(obj@model@omega == 1)]),
#                              nbCluster = obj@model@g,
#                              models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_Bk")
#     )
#     if ((discrim@error == 1)||( discrim@bestResult@likelihood==0)){
#       tmp <- VSLCM_XEM(as.matrix(obj@data[, which(obj@model@omega == 1)]), obj@partitions@zOPT, obj@model@g)
#       obj@partitions@tik <- tmp$tik
#       obj@partitions@zMAP <- tmp$z
#       obj@criteria@likelihood <- tmp$loglike
#       obj@criteria@BIC <- tmp$bic
#       obj@parameters@proportions <- tmp$pi
#       obj@parameters@means[,which(obj@model@omega == 1)]<- tmp$means
#       obj@parameters@variances[, which(obj@model@omega == 1)]<- tmp$variances
#     }else{
#       obj@criteria@likelihood <- discrim@bestResult@likelihood
#       obj@criteria@BIC <- -discrim@bestResult@criterionValue/2
#       obj@partitions@zMAP <- discrim@bestResult@partition
#       obj@partitions@tik <- discrim@bestResult@proba
#       obj@parameters@proportions <- discrim@bestResult@parameters@proportions
#       obj@parameters@means[,which(obj@model@omega == 1)]<- discrim@bestResult@parameters@mean
#       for (k in 1:nrow(obj@parameters@variances)){
#         for (it in 1:sum(obj@model@omega)){
#           obj@parameters@variances[k, which(obj@model@omega == 1)[it]]<- discrim@bestResult@parameters@variance[[k]][it, it]
#         }
#       }
#     }
#     
#   }
#   
#   if (any(obj@model@omega == 0) ){
#     discrim <- mixmodCluster(data = as.data.frame(obj@data[, which(obj@model@omega == 0)]),
#                              nbCluster = 1,
#                              models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_Bk")
#     )
#     if (discrim@error == 0){
#       
#       
#       obj@criteria@likelihood <- obj@criteria@likelihood + discrim@bestResult@likelihood
#       obj@criteria@BIC <- obj@criteria@BIC - discrim@bestResult@criterionValue/2
#       for (k in 1:obj@model@g){
#         obj@parameters@means[k, which(obj@model@omega == 0)] <- discrim@bestResult@parameters@mean
#         for (it in 1:sum(1 - obj@model@omega)){
#           obj@parameters@variances[k, which(obj@model@omega == 0)[it]]<- discrim@bestResult@parameters@variance[[1]][it, it]
#         }
#       }
#     }else{
#       for (j in  which(obj@model@omega == 0)){
#         me <- mean(obj@data[,j])
#         va <- var(obj@data[,j])
#         loglike <- sum( dnorm(obj@data[,j], me, sqrt(va), log = TRUE))
#         obj@criteria@likelihood <- obj@criteria@likelihood + loglike
#         obj@criteria@BIC <- obj@criteria@BIC + loglike - log(nrow(obj@data))
#       }     
#     }
#   }
#   
#   
#   
#   obj@criteria@ICL <- obj@criteria@BIC
#   if ((obj@model@g > 1) && (sum(obj@model@omega)>0)){
#     for (k in 1:obj@model@g)
#       obj@criteria@ICL <- obj@criteria@ICL + sum(log(obj@partitions@tik[which(obj@partitions@zMAP==k), k]))
#     
#   }
#   
#   
#   return(obj)
# }
# 

VarSelParamEstim <- function(obj){
  
  obj@criteria@likelihood <- 0
  obj@criteria@BIC <- 0
  obj@criteria@ICLbic <- 0
  obj@criteria@ICLexact <- 0
  
  obj@parameters@means <- matrix(0, obj@model@g, ncol(obj@data))
  rownames(obj@parameters@means ) <- paste("Class", 1:obj@model@g)
  obj@parameters@variances <- matrix(0, obj@model@g, ncol(obj@data))
  rownames(obj@parameters@variances ) <- paste("Class", 1:obj@model@g)
  
  obj@partitions@zOPT <- as.numeric(obj@partitions@zOPT) + 1
  
  
  if (sum(obj@model@omega) > 0){
    if (sum(obj@model@omega)==1){
      discrim <- try(Mclust(data = as.data.frame(obj@data[, which(obj@model@omega == 1)]), G = obj@model@g, modelNames = "V"), silent=TRUE)
    }else{
      discrim <- try(Mclust(data = as.data.frame(obj@data[, which(obj@model@omega == 1)]), G = obj@model@g, modelNames = "VVI"), silent=TRUE)
      if (class(discrim) == "try-error")
        discrim <- try(Mclust(data = as.data.frame(obj@data[, which(obj@model@omega == 1)]), G = obj@model@g, modelNames = c("EII", "VII","EEI", "EVI", "VVI")), silent=TRUE)
    }
    if (class(discrim)=="Mclust"){
      obj@criteria@likelihood <- discrim$loglik
      obj@criteria@BIC <- discrim$loglik - (obj@model@g * 2 * sum(obj@model@omega) + (obj@model@g-1)) * 0.5 * log(nrow(obj@data))
      obj@partitions@zMAP <- discrim$classification
      obj@partitions@tik <- discrim$z
      obj@parameters@proportions <- discrim$parameters$pro
      obj@parameters@means[,which(obj@model@omega == 1)]<- t(discrim$parameters$mean)
      if (sum(obj@model@omega)==1){
        obj@parameters@variances[,which(obj@model@omega == 1) ] <- discrim$parameters$variance$sigmasq
      }else{
        for (k in 1:nrow(obj@parameters@variances))
          obj@parameters@variances[k,which(obj@model@omega == 1) ]<- diag(discrim$parameters$variance$sigma[,,k])  
      }
    }else{
      print("error in the parameter estimation")
      obj@criteria@BIC <- -Inf
      obj@criteria@likelihood <- -Inf
      obj@partitions@tik <- matrix(1, nrow(obj@data), obj@model@g)
    }
    
  }
  
  if (any(obj@model@omega == 0) ){
    for (j in  which(obj@model@omega == 0)){
      me <- mean(obj@data[,j])
      va <- var(obj@data[,j])
      obj@parameters@means[,j] <- me
      obj@parameters@variances[,j] <- va
      loglike <- sum( dnorm(obj@data[,j], me, sqrt(va), log = TRUE))
      obj@criteria@likelihood <- obj@criteria@likelihood + loglike
      obj@criteria@BIC <- obj@criteria@BIC + loglike - log(nrow(obj@data))
    }
  }
  
  obj@criteria@ICLbic <- obj@criteria@BIC
  if ((obj@model@g > 1) && (sum(obj@model@omega)>0)){
    for (k in 1:obj@model@g)
      obj@criteria@ICLbic<- obj@criteria@ICLbic + sum(log(obj@partitions@tik[which(obj@partitions@zMAP==k), k]))  
  }  
  obj@criteria@ICLexact <-  Integre_Complete_Like(obj)
  
  return(obj)
}

VarSelCluster <- function(x, g, nbinit=50, parallel = TRUE){
  models <- VarSelModelSelection(x, g, nbinit, parallel)
  return(  VarSelParamEstim(models) )
}

VarSelModelKnown <- function(x, g, omega)
  return( VarSelParamEstim(OptimizeMICL(VarSelStartingPoint(x, g, omega), 0)))
