

setGeneric ( name= "withoutmixture",  def = function(obj){ standardGeneric("withoutmixture")})
## Pour les variables continues
setMethod( f = "withoutmixture", 
           signature(obj="VSLCMresultsContinuous"), 
           definition = function(obj){
             obj@model@omega <-  as.numeric(obj@model@omega)
             names(obj@model@omega) <- colnames(obj@data@data)
             # On remet les valeurs manquantes
             for (j in 1:obj@data@d){
               if (any(obj@data@notNA[,j]==0))
                 obj@data@data[which(obj@data@notNA[,j]==0),j] <- NA
             }
             
             obj@param@pi <- 1
             obj@param@mu <- matrix(NA, obj@data@d, 1)
             obj@param@sd <- matrix(NA, obj@data@d, 1)
             rownames(obj@param@mu)  <-   colnames(obj@data@data)
             rownames(obj@param@sd)  <-   colnames(obj@data@data)
             colnames(obj@param@mu) <-  paste("class-",1:length(obj@param@pi),sep="")
             colnames(obj@param@sd) <-  paste("class-",1:length(obj@param@pi),sep="")
             proba <- rep(0, obj@data@n)
             for (j in 1:obj@data@d){
               obj@param@mu[j,1] <- mean(obj@data@data[which(obj@data@notNA[,j]==1),j])
               obj@param@sd[j,1] <- sd(obj@data@data[which(obj@data@notNA[,j]==1),j])
               proba[which(obj@data@notNA[,j]==1)] <- proba[which(obj@data@notNA[,j]==1)] + dnorm(obj@data@data[which(obj@data@notNA[,j]==1),j], obj@param@mu[j,1], obj@param@sd[j,1], log = TRUE)
             }
             obj@partitions@zMAP <- rep(1, obj@data@n)
             obj@partitions@zOPT <- rep(1, obj@data@n)
             obj@partitions@tik <- matrix(1, obj@data@n, 1)
             obj@criteria@loglikelihood <- sum(proba)
             obj@criteria@BIC <- obj@criteria@loglikelihood - obj@data@d*log(obj@data@n)
             obj@criteria@ICL <- ICLcontinuous(obj) 
             obj@criteria@MICL <- obj@criteria@ICL
             obj@criteria@degeneracyrate <- 0
             return(obj)         
           }
)
## Pour les variables catégorielles
setMethod( f = "withoutmixture", 
           signature(obj="VSLCMresultsCategorical"), 
           definition = function(obj){
             obj@model@omega <-  as.numeric(obj@model@omega)
             names(obj@model@omega) <- colnames(obj@data@data)
             obj@param@pi <- 1
             obj@param@alpha <- list()
             proba <- rep(0, obj@data@n)
             nbparam <- 0
             for (j in 1:obj@data@d){
               obj@param@alpha[[j]] <- as.numeric(table(obj@data@data[,j])/sum(table(obj@data@data[,j])))
               who <- which(is.na(obj@data@data[,j])==FALSE)
               proba[who] <- proba[who] + log(obj@param@alpha[[j]][obj@data@data[who,j]])
               nbparam <- nbparam + length(obj@param@alpha[[j]])-1
             }
             obj@partitions@zMAP <- rep(1, obj@data@n)
             obj@partitions@zOPT <- rep(1, obj@data@n)
             obj@partitions@tik <- matrix(1, obj@data@n, 1)
             obj@criteria@loglikelihood <- sum(proba)
             obj@criteria@BIC <- obj@criteria@loglikelihood - 0.5*nbparam*log(obj@data@n)
             obj@criteria@ICL <- ICLcategorical(obj) 
             obj@criteria@MICL <- obj@criteria@ICL
             obj@criteria@degeneracyrate <- 0
             
             
             
             # On remet les valeurs manquantes
             if (any(obj@data@shortdata == 0)){
               for (j in 1:ncol(obj@data@shortdata))
                 obj@data@shortdata[which(obj@data@shortdata[,j] == 0), ] <- NA
             }
             
             
             colnames(obj@partitions@tik )=paste("class-",1:obj@model@g,sep="")
             
             
             
             return(obj)           
           }
)
## Pour les variables mixed
setMethod( f = "withoutmixture", 
           signature(obj="VSLCMresultsMixed"), 
           definition = function(obj){
             obj@model@omega <-  as.numeric(obj@model@omega)
             namestmp <- c(colnames(obj@data@dataContinuous@data),colnames(obj@data@dataCategorical@shortdata))
             names(obj@model@omega) <- namestmp
             
             obj@param@pi <- 1
             proba <- rep(0, obj@data@n)
             nbparam <- obj@data@dataContinuous@d*2
             
             obj@param@paramContinuous@pi <- 1
             obj@param@paramContinuous@mu <- matrix(NA, obj@data@dataContinuous@d, 1)
             obj@param@paramContinuous@sd <- matrix(NA, obj@data@dataContinuous@d, 1)
             rownames(obj@param@paramContinuous@mu)  <-   colnames(obj@data@dataContinuous@data)
             rownames(obj@param@paramContinuous@sd)  <-   colnames(obj@data@dataContinuous@data)
             colnames(obj@param@paramContinuous@mu) <-  paste("class-",1:length(obj@param@paramContinuous@pi),sep="")
             colnames(obj@param@paramContinuous@sd) <-  paste("class-",1:length(obj@param@paramContinuous@pi),sep="")
             proba <- rep(0, obj@data@dataContinuous@n)
             for (j in 1:obj@data@dataContinuous@d){
               obj@param@paramContinuous@mu[j,1] <- mean(obj@data@dataContinuous@data[which(obj@data@dataContinuous@notNA[,j]==1),j])
               obj@param@paramContinuous@sd[j,1] <- sd(obj@data@dataContinuous@data[which(obj@data@dataContinuous@notNA[,j]==1),j])
               proba[which(obj@data@dataContinuous@notNA[,j]==1)] <- proba[which(obj@data@dataContinuous@notNA[,j]==1)] + dnorm(obj@data@dataContinuous@data[which(obj@data@dataContinuous@notNA[,j]==1),j], obj@param@paramContinuous@mu[j,1], obj@param@paramContinuous@sd[j,1], log = TRUE)
             }             
             obj@param@paramCategorical@pi <- 1
             obj@param@paramCategorical@alpha <- list()
             for (j in 1:obj@data@dataCategorical@d){
               obj@param@paramCategorical@alpha[[j]] <- as.numeric(table(obj@data@dataCategorical@data[,j])/sum(table(obj@data@dataCategorical@data[,j])))
               who <- which(is.na(obj@data@dataCategorical@data[,j])==FALSE)
               proba[who] <- proba[who] + log(obj@param@paramCategorical@alpha[[j]][obj@data@dataCategorical@data[who,j]])
               nbparam <- nbparam + length(obj@param@paramCategorical@alpha[[j]])-1
             }
             obj@partitions@zMAP <- rep(1, obj@data@n)
             obj@partitions@zOPT <- rep(1, obj@data@n)
             obj@partitions@tik <- matrix(1, obj@data@n, 1)
             colnames(obj@partitions@tik) <- paste("class-",1:obj@model@g,sep="")
             obj@criteria@loglikelihood <- sum(proba)
             obj@criteria@ICL <- ICLmixed(obj) 
             obj@criteria@MICL <- obj@criteria@ICL
             obj@criteria@degeneracyrate <- 0                         
             nbparam <- obj@model@g-1 
             if (obj@data@withContinuous)
               nbparam <- nbparam + obj@data@dataContinuous@d* 2
             
             if (obj@data@withCategorical){
               for (j in 1:obj@data@dataCategorical@d)
                 nbparam <- nbparam + (length(obj@data@dataCategorical@modalitynames[[j]])-1)
               
             }
             obj@criteria@BIC <- obj@criteria@loglikelihood - nbparam*0.5*log(obj@data@n)
             
             return(obj)           
           }
)
