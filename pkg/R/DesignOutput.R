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
                 reference@partitions@zOPT <- as.numeric(reference@partitions@zOPT) + 1
                 # Calcul des proba post
                 proba <- matrix(log(reference@param@pi), reference@data@n, reference@model@g, byrow = T)
                 for (j in 1:reference@data@d){
                   who <- which(reference@data@notNA[,j]==1)
                   for (k in 1:ncol(proba))
                     proba[who, k] <- proba[who, k] + dnorm(reference@data@data[who,j], reference@param@mu[j,k], reference@param@sd[j,k], log = TRUE)
                 }
                 tmpmax <- apply(proba, 1, "max")
                 proba <- exp(sweep(proba, 1, tmpmax, "-"))
                 reference@partitions@tik <- proba/rowSums(proba)
                 colnames(reference@partitions@tik ) <-  paste("class-",1:reference@model@g,sep="")
                 reference@partitions@zMAP <- apply(proba,1,which.max)
                 # Calcul des criteres d'information
                 reference@criteria@likelihood <- sum(log(rowSums(proba))) + sum(tmpmax)
                 reference@criteria@BIC <- reference@criteria@likelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
                 reference@criteria@ICL <- ICLcontinuous(reference) 
               }else{
                 warning("All the models get error (degeneracy)", call. = FALSE)
                 reference@criteria@likelihood <- -Inf
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
             if (reference@strategy@paramEstim == TRUE){
               reference@model@omega <-  as.numeric(reference@model@omega)
               names(reference@model@omega) <- colnames(reference@data@data)
               #              if (reference@strategy@vbleSelec==FALSE){
               #                reference@partitions@zOPT <- numeric()
               #                reference@criteria@MICL <- numeric()
               #              }
               # Attention zOPT correspond aux profiles, on repasse donc au niveau des individus
               if (reference@strategy@vbleSelec==TRUE){
                 reference@partitions@zOPT <- as.numeric(reference@partitions@zOPT) + 1
                 reference@partitions@zOPT <- reference@partitions@zOPT[attr(reference@data@shortdata,"index")]
               }
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
               # Calcul des proba post
               proba <- matrix(reference@param@pi, reference@data@n, reference@model@g, byrow = T)
               for (j in 1:reference@data@d){
                 who <- which(is.na(reference@data@data[,j]) == FALSE )
                 proba[who, ] <- proba[who, ] * t(reference@param@alpha[[j]][,reference@data@data[who,j]])
               }   
               reference@partitions@tik <- proba/rowSums(proba)  
               colnames(reference@partitions@tik )=paste("class-",1:reference@model@g,sep="")
               reference@partitions@zMAP <- apply(proba,1,which.max)
               # Calcul des criters d'information
               reference@criteria@likelihood <- sum(log(rowSums(proba)))
               reference@criteria@BIC <- reference@criteria@likelihood  - 0.5*(reference@model@g-1 + reference@model@g*2*sum(reference@model@omega) + 2*sum(1-reference@model@omega))*log(reference@data@n)
               reference@criteria@ICL <- ICLcategorical(reference) 
             }
             return(reference)
           }
)