########################################################################################################################
## Déclaration et construction des classes S4 relatives aux données
########################################################################################################################


########################################################################################################################
## La classe S4 VSLCMdataContinuous est relatives à des données continues. Elle possède 5 slots:
## n: nombre d'observations
## d: nombre de variables
## data: matrix où les colonnes sont numeric et correspondent aux donées
## notNA: matrix of logical valant 1 si la réalisation est observée et 0 sinon
## priors: valeur des priors pour chaque variable (en ligne)
########################################################################################################################
setClass(
  Class = "VSLCMdataContinuous", 
  representation = representation(
    n="numeric",
    d="numeric",
    data="matrix",
    notNA="matrix",
    priors="matrix"
  ), 
  prototype = prototype(
    n=numeric(),
    d=numeric(),
    data=matrix(),
    notNA=matrix(),
    priors=matrix()
  )
)

########################################################################################################################
## La classe S4 VSLCMdataCategorical est relatives à des données catégorielles. Elle possède 6 slots:
## n: nombre d'observations
## d: nombre de variables
## data: matrix où les facteurs orginiaux ont été converti en numéric
## shortdata: matrix contenant les profils uniques
## weightdata: poids de chaque profil
## modalitynames: list contenant les noms de modalités pour chaque variable
########################################################################################################################
setClass(
  Class = "VSLCMdataCategorical", 
  representation = representation(
    n="numeric",
    d="numeric",
    data="matrix",
    shortdata="matrix",
    weightdata="numeric",
    modalitynames="list"
  ), 
  prototype = prototype(
    n=numeric(),
    d=numeric(),
    data=matrix(),
    shortdata=matrix(),
    weightdata=numeric(),
    modalitynames=list()
  )
)


########################################################################################################################
## La classe S4 VSLCMdataMixed est relatives à des données mixtes. Elle possède une liste d'élèments
## de classe S4 VSLCMdataContinuous ou VSLCMdataCategorical
########################################################################################################################
setClass(
  Class = "VSLCMdataMixed", 
  representation = representation(
    n="numeric",
    d="numeric",
    data="list"
  ), 
  prototype = prototype(
    n=numeric(),
    d=numeric(),
    data=list()
  )
)

########################################################################################################################
## La fonction VSLCMdata permet de construire un objet de class S4 VSLCMdataContinuous ou VSLCMdataCategorical en fonction
## de la nature des variables
########################################################################################################################
VSLCMdata <- function(x){
  # Ajout d'un nom de variable si celui-ci est manquant
  if (is.null(colnames(x)))
    colnames(x) <- paste("X",1:ncol(x), sep="")
  
  n <- nrow(x)
  d <- ncol(x)
  # recherche des indices de variables numeric et factor
  idxcat <- numeric()
  idxcont <- numeric()
  mat <- matrix(NA, n, d)
  for (j in 1:d){
    if (class(x[,j])=="factor")
      idxcat <- c(idxcat,j)
    else if (class(x[,j])=="numeric")
      idxcont <- c(idxcont, j)
    else
      stop("At least one variable is not factor or integer or numeric!")
    mat[, j] <- as.numeric(x[,j])
  }
  # cas des variables categorielles
  if (length(idxcat) == d){
    shortdata <- mat
    ## Pour travailler avec Armadillo on rempli artificellement les NA par 0
    shortdata[is.na(shortdata)] <- 0
    shortdata <- uniquecombs(shortdata)
    weightdata <- as.numeric(table(attr(shortdata,"index")))
    colnames(shortdata) <- colnames(x)
    modalitynames <- list()
    for (j in 1:d){
      modalitynames[[j]] <- levels(x[,j])
      if (length(modalitynames[[j]]) != length(unique(x[which(is.na(x[,j])==FALSE),j])))
        stop(paste("The number of observed modalities is not equal to the number of levels for variable", colnames(x)[j]))
    }
    output <-  new("VSLCMdataCategorical", n=n, d=d, data=mat, shortdata=shortdata,
                   weightdata=weightdata, modalitynames=modalitynames)
  }else  if (length(idxcont) == d){ 
    # construction des priors
    priors <- matrix()
    if (length(idxcont) != 0){
      priors <- matrix(NA, d, 4)
      colnames(priors) <- c("alpha", "beta", "lambda", "delta")
#       for (j in idxcont){
#         priors[j,1] <- 1.28*2
#         priors[j,2] <- sqrt(0.72 * var(x[,j], na.rm = T))
#         priors[j,3] <- mean(x[,j], na.rm = T)
#         priors[j,4] <- 2.6 /(max(x[,j], na.rm = T) - min(x[,j], na.rm = T))
#       }
      # Attention ici on met les priors a 1!!!!
      priors <- matrix(1, d, 4)
    }
    ## Pour travailler avec Armadillo on rempli artificellement les NA par 0
    notNA <- (is.na(x)==FALSE)*1
    mat[is.na(mat)] <- 0
    colnames(mat) <-  colnames(x)
    colnames(notNA) <- colnames(x)
    output <-  new("VSLCMdataContinuous", n=n, d=d, data=mat, notNA=notNA, priors=priors)    
  }else{
    output <- list()
    if (length(idxcont) != 0)
      output$continuous <- VSLCMdata(x[, idxcont])
    if (length(idxcat) != 0)
      output$categorical <- VSLCMdata(x[, idxcat])
    output <- new("VSLCMdataMixed", n=n, d=d, data=output)
  }
 #   stop("Data set is not correct!")
  
  return(output)
}
