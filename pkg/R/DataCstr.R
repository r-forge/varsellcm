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
## La classe S4 VSLCMdataInteger est relatives à des données entières Elle possède 5 slots:
## n: nombre d'observations
## d: nombre de variables
## data: matrix où les colonnes sont numeric et correspondent aux donées
## notNA: matrix of logical valant 1 si la réalisation est observée et 0 sinon
## priors: valeur des priors pour chaque variable (en ligne)
########################################################################################################################
setClass(
  Class = "VSLCMdataInteger", 
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

########################################################################################################################
setClass(
  Class = "VSLCMdataMixed", 
  representation = representation(
    n="numeric",
    d="numeric",
    withContinuous="logical",
    withInteger="logical",
    withCategorical="logical",
    dataContinuous="VSLCMdataContinuous",
    dataInteger="VSLCMdataInteger",
    dataCategorical="VSLCMdataCategorical"
  ), 
  prototype = prototype(
    n=numeric(),
    d=numeric(),
    withContinuous=logical(),
    withInteger=logical(),
    withCategorical=logical(),
    dataContinuous=new("VSLCMdataContinuous"),
    dataInteger=new("VSLCMdataInteger"),
    dataCategorical=new("VSLCMdataCategorical")
  )
)

########################################################################################################################
## La fonction VSLCMdata permet de construire un objet de class S4 VSLCMdataContinuous ou VSLCMdataCategorical en fonction
## de la nature des variables
########################################################################################################################
VSLCMdata <- function(x, redquali=TRUE){
  # Ajout d'un nom de variable si celui-ci est manquant
  if (is.null(colnames(x))) colnames(x) <- paste("X",1:ncol(x), sep="")
  
  n <- nrow(x)
  d <- ncol(x)
  # recherche des indices de variables numeric et factor
  type <- numeric()
  for (j in 1:d) type[j] <- class(x[,j])
  idxcont <- which(type=="numeric")
  idxinte <- which(type=="integer")
  idxcat <- which(type=="factor")
  if ((all(type %in% c("numeric", "integer", "factor"))==FALSE))
    stop("At least one variable is neither numeric, integer nor factor!")
  mat <- apply(x, 2, as.numeric)
  
  # cas des variables categorielles
  if (length(idxcat) == d){
    shortdata <- mat
    weightdata <- rep(1, n)
    ## Pour travailler avec Armadillo on rempli artificellement les NA par 0
    shortdata[is.na(shortdata)] <- 0
    if (redquali==TRUE){
      shortdata <- uniquecombs(shortdata)
      weightdata <- as.numeric(table(attr(shortdata,"index")))
    }
    colnames(shortdata) <- colnames(x)
    modalitynames <- list()
    for (j in 1:d){
      modalitynames[[j]] <- levels(x[,j])
      if (length(modalitynames[[j]]) != length(unique(x[which(is.na(x[,j])==FALSE),j])))
        stop(paste("The number of observed modalities is not equal to the number of levels for variable", colnames(x)[j]))
    }
    output <-  new("VSLCMdataCategorical", n=n, d=d, data=mat, shortdata=shortdata, weightdata=weightdata, modalitynames=modalitynames)
  }else  if (length(idxcont) == d){ 
    # construction des priors
    priors <- matrix(1, d, 4)
    priors[,4] <- 1/100
    priors[,3] <- colMeans(x, na.rm = T)
    colnames(priors) <- c("alpha", "beta", "lambda", "delta")
    ## Pour travailler avec Armadillo on rempli artificellement les NA par 0
    notNA <- (is.na(x)==FALSE)*1
    mat[is.na(mat)] <- 0
    colnames(mat) <-  colnames(x)
    colnames(notNA) <- colnames(x)
    output <-  new("VSLCMdataContinuous", n=n, d=d, data=mat, notNA=notNA, priors=priors)    
  } else  if (length(idxinte) == d){ 
    # construction des priors
    priors <- matrix(1, d, 2)
    colnames(priors) <- c("alpha", "beta")
    ## Pour travailler avec Armadillo on rempli artificellement les NA par 0
    notNA <- (is.na(x)==FALSE)*1
    mat[is.na(mat)] <- 0
    colnames(mat) <-  colnames(x)
    colnames(notNA) <- colnames(x)
    output <-  new("VSLCMdataInteger", n=n, d=d, data=mat, notNA=notNA, priors=priors)    
  }else{
    output <- list(continuous=new("VSLCMdataContinuous"), integer=new("VSLCMdataInteger"), categorical=new("VSLCMdataCategorical"))
    if (length(idxcont) != 0){
      tmpdata <- data.frame(x[,idxcont])
      colnames(tmpdata) <- colnames(x)[idxcont]
      output$continuous <- VSLCMdata(tmpdata)
    }
    if (length(idxinte) != 0){
      tmpdata <- data.frame(x[,idxinte])
      colnames(tmpdata) <- colnames(x)[idxinte]
      output$integer <- VSLCMdata(tmpdata)
    }
    if (length(idxcat) != 0){
      tmpdata <- data.frame(x[,idxcat])
      colnames(tmpdata) <- colnames(x)[idxcat]      
      output$categorical <- VSLCMdata(tmpdata, redquali=FALSE)
    }
    
    output <- new("VSLCMdataMixed", n=n, d=d, 
                  withContinuous=(length(idxcont) != 0),  withInteger=(length(idxinte) != 0), withCategorical=(length(idxcat) != 0),
                  dataContinuous=output$continuous, dataInteger=output$integer, dataCategorical=output$categorical)
  }
  return(output)
}