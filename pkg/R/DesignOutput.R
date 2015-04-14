########################################################################################################################
## La fonction DesignOutput permet de mettre en forme les paramètres en fonction de leur nature VSLCMresultsContinuous
## ou VSLCMresultsCategorical. Elle est appelée à la fin de l'estimation des paramètres
########################################################################################################################
setGeneric ( name= "DesignOutput",  def = function(reference){ standardGeneric("DesignOutput")})

## Cas de variables continues
setMethod( f = "DesignOutput", 
           signature(reference="VSLCMresultsContinuous"), 
           definition = function(reference){
             reference@model@omega <-  as.numeric(reference@model@omega)
             names(reference@model@omega) <- colnames(reference@data@data)
             if (reference@strategy@vbleSelec==FALSE){
               reference@partitions@zOPT <- numeric()
               reference@criteria@MICL <- numeric()
             }
             if (reference@strategy@paramEstim){
               rownames(reference@param@mu)  <-   colnames(reference@data@data)
               rownames(reference@param@sd)  <-   colnames(reference@data@data)
               reference@param@pi <- as.numeric(reference@param@pi)
               names(reference@param@pi) <-  paste("class-",1:length(reference@param@pi),sep="")
               colnames(reference@param@mu) <-  paste("class-",1:length(reference@param@pi),sep="")
               colnames(reference@param@sd) <-  paste("class-",1:length(reference@param@pi),sep="")
               if (all(reference@param@sd != 0)){
                 reference@partitions@zMAP <- as.numeric(reference@partitions@zMAP) + 1
                 reference@partitions@zOPT <- as.numeric(reference@partitions@zOPT) + 1
                 colnames(reference@partitions@tik) <-  paste("class-",1:reference@model@g,sep="")
                 reference@criteria@BIC <- reference@criteria@loglikelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
                 reference@criteria@ICL <- ICLcontinuous(reference) 
               }else{
                 warning("All the models get error (degeneracy)", call. = FALSE)
                 reference@criteria@loglikelihood <- -Inf
                 reference@criteria@BIC <- -Inf
                 reference@criteria@ICL <- -Inf                 
               }
               # On remet les valeurs manquantes
               for (j in 1:reference@data@d){
                 if (any(reference@data@notNA[,j]==0))
                   reference@data@data[which(reference@data@notNA[,j]==0),j] <- NA
               }
             }
             return(reference)
           }
)
## Cas des variables categorielles
setMethod( f = "DesignOutput", 
           signature(reference="VSLCMresultsCategorical"), 
           definition = function(reference){
             reference@model@omega <-  as.numeric(reference@model@omega)
             names(reference@model@omega) <- colnames(reference@data@data)
             if (reference@strategy@paramEstim == TRUE){
               if (reference@strategy@vbleSelec==FALSE){
                 reference@partitions@zOPT <- numeric()
                 reference@criteria@MICL <- numeric()
               }else{
                 reference@partitions@zOPT <- 1 + reference@partitions@zOPT[attr(reference@data@shortdata,"index")]
                 reference@partitions@zMAP <- 1 + reference@partitions@zMAP[attr(reference@data@shortdata,"index")]
               }
               # Attention zOPT correspond aux profiles, on repasse donc au niveau des individus
               reference@param@pi <- as.numeric(reference@param@pi)
               names(reference@param@pi) <- paste("class-",1:length(reference@param@pi),sep="")
               for (j in 1:reference@data@d){
                 reference@param@alpha[[j]] <- matrix(reference@param@alpha[[j]], nrow = reference@model@g)
                 rownames(reference@param@alpha[[j]]) <- paste("class-",1:length(reference@param@pi),sep="")
                 colnames(reference@param@alpha[[j]]) <- reference@data@modalitynames[[j]]
               }
               names(reference@param@alpha) <- colnames(reference@data@shortdata)
               # On remet les valeurs manquantes
               if (any(reference@data@shortdata == 0)){
                 for (j in 1:ncol(reference@data@shortdata))
                   reference@data@shortdata[which(reference@data@shortdata[,j] == 0), ] <- NA
               }
               reference@partitions@tik <- reference@partitions@tik[attr(reference@data@shortdata,"index"),] + 1
               colnames(reference@partitions@tik )=paste("class-",1:reference@model@g,sep="")
               reference@criteria@BIC <- reference@criteria@loglikelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
               reference@criteria@ICL <- ICLcategorical(reference) 
             }
             return(reference)
           }
)

## Cas des variables mixed
setMethod( f = "DesignOutput", 
           signature(reference="VSLCMresultsMixed"), 
           definition = function(reference){
             reference@model@omega <-  as.numeric(reference@model@omega)
             namestmp <- c(colnames(reference@data@dataContinuous@data),colnames(reference@data@dataCategorical@shortdata))
             names(reference@model@omega) <- namestmp
             
             if (reference@strategy@paramEstim == TRUE){
               if (reference@strategy@vbleSelec==FALSE){
                 reference@partitions@zOPT <- numeric()
                 reference@criteria@MICL <- numeric()
               }else{
                 reference@partitions@zMAP <- as.numeric(reference@partitions@zMAP) + 1
                 reference@partitions@zOPT <- as.numeric(reference@partitions@zOPT) + 1
               }
               
               rownames(reference@param@paramContinuous@mu)  <-   colnames(reference@data@dataContinuous@data)
               rownames(reference@param@paramContinuous@sd)  <-   colnames(reference@data@dataContinuous@data)
               reference@param@paramContinuous@pi <- as.numeric(reference@param@paramContinuous@pi)
               names(reference@param@paramContinuous@pi) <-  paste("class-",1:length(reference@param@paramContinuous@pi),sep="")
               colnames(reference@param@paramContinuous@mu) <-  paste("class-",1:length(reference@param@paramContinuous@pi),sep="")
               colnames(reference@param@paramContinuous@sd) <-  paste("class-",1:length(reference@param@paramContinuous@pi),sep="")
               # On remet les valeurs manquantes
               for (j in 1:reference@data@dataContinuous@d){
                 if (any(reference@data@dataContinuous@notNA[,j]==0))
                   reference@data@dataContinuous@data[which(reference@data@dataContinuous@notNA[,j]==0),j] <- NA
               }
               if (all(reference@param@paramContinuous@sd != 0)){             
                 reference@param@pi <- as.numeric(reference@param@pi)
                 names(reference@param@pi) <- paste("class-",1:length(reference@param@pi),sep="")
                 reference@param@paramCategorical@pi <- as.numeric(reference@param@paramCategorical@pi)
                 names(reference@param@paramCategorical@pi) <-  paste("class-",1:length(reference@param@paramCategorical@pi),sep="")
                 
                 for (j in 1:reference@data@dataCategorical@d){
                   reference@param@paramCategorical@alpha[[j]] <- matrix(reference@param@paramCategorical@alpha[[j]], nrow = reference@model@g)
                   rownames(reference@param@paramCategorical@alpha[[j]]) <- paste("class-",1:length(reference@param@pi),sep="")
                   colnames(reference@param@paramCategorical@alpha[[j]]) <- reference@data@dataCategorical@modalitynames[[j]]
                 }
                 names(reference@param@paramCategorical@alpha) <- colnames(reference@data@dataCategorical@shortdata)
                 # On remet les valeurs manquantes
                 if (any(reference@data@dataCategorical@shortdata == 0)){
                   for (j in 1:ncol(reference@data@dataCategorical@shortdata))
                     reference@data@dataCategorical@shortdata[which(reference@data@dataCategorical@shortdata[,j] == 0), ] <- NA
                 }
                 reference@partitions@tik <- reference@partitions@tik + 1
                 colnames(reference@partitions@tik ) <- paste("class-",1:reference@model@g,sep="")
                 reference@criteria@BIC <- reference@criteria@loglikelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
                 reference@criteria@BIC <- reference@criteria@loglikelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
                 
                 #reference@criteria@ICL <- ICLcategorical(reference) 
               }else{
                 warning("All the models get error (degeneracy)", call. = FALSE)
                 reference@criteria@loglikelihood <- -Inf
                 reference@criteria@BIC <- -Inf
                 reference@criteria@ICL <- -Inf                 
               }
             }
             
             return(reference)
           }
)