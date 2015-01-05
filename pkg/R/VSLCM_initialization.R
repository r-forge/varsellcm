VSLCM_initialization_omega <- function(n, d, g){
  omega <- rep(0, d)
  cb <- sample(2:(min(n,d)),1)
  omega[sample(1:d, cb)] <- 1
  return(omega)
}

# VSLCM_initialization_z <- function(omega, g, x){
#   z <- rep(1, nrow(x))
#   
#   if ((g>1)&&(sum(omega)>0)){
#     filtre <- 0
#     tmp <- mixmodCluster(data = as.data.frame(x[,which(omega==1)]),
#                          nbCluster = g,
#                          models = mixmodGaussianModel(listModels = "Gaussian_pk_Lk_Bk")
#     )
#     if (tmp@error == FALSE)
#       z <- tmp@bestResult@partition
#     else
#       z <- sample(1:g, nrow(x), replace=TRUE)
#     
#   }
#   
#   return(list(z=z,error=1*tmp@error))
# }

VSLCM_initialization_z <- function(omega, g, x){
  z <- rep(1, nrow(x))
  error <- 0
  if ((g>1)&&(sum(omega)>0)){
    if (sum(omega)==1){
      test <- try(Mclust(data = as.data.frame(x[,which(omega==1)]), G = g, modelNames = "V"), silent = TRUE)    
    }else{
      test <- try(Mclust(data = as.data.frame(x[,which(omega==1)]), G = g, modelNames = "VVI"), silent = TRUE)    
    }
    if (class(test) == "Mclust"){
      z <- test$classification
    }else{
      z <- sample(1:g, nrow(x), replace=TRUE)    
      error <- 1
    }
    
  }
  
  return(list(z=z, error=error))
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
  if (missing(omega)){
    omega <- VSLCM_initialization_omega(nrow(x), ncol(x), g)
    
    if (missing(z))
      tmp <- VSLCM_initialization_z(omega, g, x)
    
    while (tmp$error == 1){
      omega <- VSLCM_initialization_omega(nrow(x), ncol(x), g)
      tmp <- VSLCM_initialization_z(omega, g, x)
    }
    z <- tmp$z
  }
  
  if (missing(z))
    z <- VSLCM_initialization_z(omega, g, x)$z
  
  if (min(z)>0)
    z <- z-1
  
  starting <- new("VSLCMresults",
                  data = as.matrix(x),
                  priors = priors,
                  criteria = new("VSLCMcriteria", likelihood=-Inf, BIC=-Inf, ICLbic=-Inf, ICLexact=-Inf, MICL=-Inf),
                  partitions = new("VSLCMpartitions", zMAP=z, zOPT=z),
                  model = new("VSLCMmodel", g=g, omega=omega),
                  parameters = new("VSLCMparameters")
  )
  return( starting )
}
