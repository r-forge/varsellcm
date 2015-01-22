VSLCM_initialization_omega <- function(n, d, g){
  omega <- rep(0, d)
  cb <- sample(2:(min(n,d)),1)
  omega[sample(1:d, cb)] <- 1
  return(omega)
}



VSLCM_initialization_z <- function(omega, g, x){
  z <- rep(1, nrow(x))
  if ((g>1)&&(sum(omega)>0)){
    if (sum(omega)==1){
      test <- try(Mclust(data = as.data.frame(x[,which(omega==1)]), G = g, modelNames = "V"), silent = TRUE)    
    }else{
      test <- try(Mclust(data = as.data.frame(x[,which(omega==1)]), G = g, modelNames = "VVI"), silent = TRUE)    
    }
    if (class(test) == "Mclust"){
      z <- test$classification
    }else{
      z <- kmeans(as.matrix(scale(x[,which(omega==1)],TRUE,TRUE)), g)$cluster
    }
    
  }
  
  return(z)
}


VSLCM_initialization_priors <- function(x){
  priors <- matrix(1, ncol(x), 4)
  colnames(priors) <- c("alpha", "beta", "lambda", "delta")
  for (j in 1:ncol(x)){
    priors[j,1] <- 1.28*2
    priors[j,2] <- sqrt(0.72 * var(x[,j]))
    priors[j,3] <- mean(x[,j])
    priors[j,4] <- 2.6 /(max(x[,j]) - min(x[,j]))
  }
  priors <- matrix(1, ncol(x), 4)
  return(priors)
}

VarSelStartingPoint <- function(x, g, omega, z, priors){
  if (missing(priors))
    priors <- VSLCM_initialization_priors(as.matrix(x))
  
  # Initialization
  if (missing(omega))
    omega <- VSLCM_initialization_omega(nrow(x), ncol(x), g)
    
  if (missing(z))
    z <- VSLCM_initialization_z(omega, g, x)
  
  if (min(z)>0)
    z <- z-1
  
  if (is.null(colnames(x)))
    colnames(x) <- paste("X",1:ncol(x), sep="")
  
  
  
  starting <- new("VSLCMresults",
                  data = as.matrix(x),
                  priors = priors,
                  criteria = new("VSLCMcriteria", likelihood=-Inf, BIC=-Inf, ICL=-Inf, MICL=-Inf),
                  partitions = new("VSLCMpartitions", zMAP=z, zOPT=z),
                  model = new("VSLCMmodel", g=g, omega=omega),
                  parameters = new("VSLCMparameters")
  )
  print("fin init")
  return( starting )
}


# VSLCM_initialization_omega <- function(n, d, g){
#   omega <- rep(0, d)
#   cb <- sample(2:(min(n,d)),1)
#   omega[sample(1:d, cb)] <- 1
#   return(omega)
# }
# 
# 
# 
# 
# VSLCM_initialization_priors <- function(x){
#   priors <- matrix(1, ncol(x), 4)
#   colnames(priors) <- c("alpha", "beta", "lambda", "delta")
#   for (j in 1:ncol(x)){
#     priors[j,1] <- 1.28*2
#     priors[j,2] <- sqrt(0.72 * var(x[,j]))
#     priors[j,3] <- mean(x[,j])
#     priors[j,4] <- 2.6 /(max(x[,j]) - min(x[,j]))
#   }
#   priors <- matrix(1, ncol(x), 4)
#   return(priors)
# }
# 
# VarSelStartingPoint <- function(x, g, omega, z, priors){
#   if (missing(priors))
#     priors <- VSLCM_initialization_priors(as.matrix(x))
#   
#   if (missing(omega))
#     omega <- VSLCM_initialization_omega(nrow(x), ncol(x), g)
#   
#   if (missing(z)){
#     print("ici")
#     test <- try(mixmodCluster(as.data.frame(x[,which(omega==1)]),
#                               g,
#                               models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_Bk")
#                               ), silent = TRUE)    
#     print("la")
#     if (class(pt3) == "MixmodCluster"){
#       print("ici")
#       z <- test@bestResult@partition
#     }else{
#       print("l")
#       z <- kmeans(scale(as.matrix(x[,which(omega==1)]), TRUE, TRUE), g)$cluster
#     }
#   }
#     
#   
#   if (is.null(colnames(x)))
#     colnames(x) <- paste("X",1:ncol(x), sep="")
#       
#   starting <- new("VSLCMresults",
#                   data = as.matrix(x),
#                   priors = priors,
#                   criteria = new("VSLCMcriteria", likelihood=-Inf, BIC=-Inf, ICL=-Inf, MICL=-Inf),
#                   partitions = new("VSLCMpartitions", zMAP=z, zOPT=z),
#                   model = new("VSLCMmodel", g=g, omega=omega),
#                   parameters = new("VSLCMparameters")
#   )
#   return( starting )
# }
