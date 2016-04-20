########################################################################################################################
## Fonctions principales du package, les seules accessibles par l'utilisateur sont VarSelCluster,
## Imputation (voir Imputation.R) et MICL
########################################################################################################################
setGeneric ( name= "MICL",  def = function(x, obj){ standardGeneric("MICL")})
## Pour les variables continues
setMethod( f = "MICL", 
           signature(x="data.frame", obj="VSLCMresultsContinuous"), 
           definition = function(x, obj){
             obj@strategy@crit.varsel <- TRUE
             obj@data  <- VSLCMdata(x)
             tmp <- ComputeMICL(obj, "Continuous")
             return(list(MICL=tmp@criteria@MICL, zOPT=tmp@partitions@zOPT+1))         
           }
)
## Pour les variables entieres
setMethod( f = "MICL", 
           signature(x="data.frame", obj="VSLCMresultsInteger"), 
           definition = function(x, obj){
             obj@strategy@crit.varsel <- TRUE
             # travail sur les donnees manquantes
             obj@data  <- VSLCMdata(x)
             tmp <- ComputeMICL(obj, "Integer")
             return(list(MICL=tmp@criteria@MICL, zOPT=tmp@partitions@zOPT+1))          
           }
)
## Pour les variables categorielles
setMethod( f = "MICL", 
           signature(x="data.frame", obj="VSLCMresultsCategorical"), 
           definition = function(x, obj){
             obj@strategy@crit.varsel <- TRUE
             obj@data  <- VSLCMdata(x)
             tmp <- ComputeMICL(obj, "Categorical")
             tmp@partitions@zOPT <-  1 + as.numeric(obj@partitions@zOPT[attr(obj@data@shortdata,"index")])
             return(list(MICL=tmp@criteria@MICL, zOPT=tmp@partitions@zOPT))       
           }
)
## Pour les variables mixed
setMethod( f = "MICL", 
           signature(x="data.frame", obj="VSLCMresultsMixed"), 
           definition = function(x, obj){
             obj@strategy@crit.varsel <- TRUE
             obj@data  <- VSLCMdata(x)
             tmp <- ComputeMICL(obj, "Mixed")
             return(list(MICL=tmp@criteria@MICL, zOPT=tmp@partitions@zOPT+1))     
           }
)


########################################################################################################################
## La fonction VarSelModelMLE permet d'effectuer l'estimation des parametres en considerant que les variables donnees
## dans le slot model de l'objet VSLCMresultsContinuous ou VSLCMresultsCategorical.
## Il appelle le code c++ et retourne un objet VSLCMresultsContinuous ou VSLCMresultsCategorical en fonction de la
## nature des donnees.
########################################################################################################################
setGeneric ( name= "VarSelModelMLE",  def = function(obj,it){ standardGeneric("VarSelModelMLE")})
## Pour les variables continues
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsContinuous",it="numeric"), 
           definition = function(obj,it){
             if (obj@strategy@crit.varsel=="MICL"){
               reference <- OptimizeMICL(obj, "Continuous")               
             }else{
               stop("error")
             }
             return(reference)         
           }
)
## Pour les variables entiers
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsInteger",it="numeric"), 
           definition = function(obj,it){
             if (obj@strategy@crit.varsel=="MICL"){
               reference <- OptimizeMICL(obj, "Integer")
             }else{
               stop("error")
             }
             return(reference)         
           }
)
## Pour les variables categorielles
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsCategorical",it="numeric"), 
           definition = function(obj, it){
             if (obj@strategy@crit.varsel=="MICL"){
               reference <- OptimizeMICL(obj, "Categorical")
             }else{
               stop("error")
             }
             return(reference)           
           }
)
## Pour les variables mixed
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsMixed",it="numeric"), 
           definition = function(obj, it){
             if (obj@strategy@crit.varsel=="MICL"){
               reference <- OptimizeMICL(obj, "Mixed")
             }else{
               pen <- 0
               if (obj@strategy@crit.varsel=="AIC") pen <- 1
               if (obj@strategy@crit.varsel=="BIC") pen <- 0.5*log(obj@data@n)
               reference <- OptimizePenLike(obj, pen)
             }
             return(reference)           
           }
)

BuildS4Reference <- function(x, g, initModel, vbleSelec, crit.varsel, paramEstim, nbcores, discrim, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep){
  CheckInputs(x, g, initModel, vbleSelec, crit.varsel, discrim, paramEstim, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  # Creation de l'objet S4 VSLCMstrategy contenant les parametres de reglage
  strategy <- VSLCMstrategy(initModel, nbcores, vbleSelec, crit.varsel, paramEstim, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)    
  # Creation de l'objet S4 VSLCMdataContinuous ou VSLCMdataCategorical
  if ((vbleSelec==FALSE) || (crit.varsel=="MICL"))
    data <- VSLCMdata(x)
  else
    data <- VSLCMdataMixte(x)
  
  
  if (class(data) == "VSLCMdataContinuous")
    reference <- new("VSLCMresultsContinuous", data=data, criteria=InitCriteria(), model=new("VSLCMmodel",g=g, omega=discrim), strategy=strategy)
  else if (class(data) == "VSLCMdataInteger")
    reference <- new("VSLCMresultsInteger", data=data, criteria=InitCriteria(), model=new("VSLCMmodel",g=g, omega=discrim), strategy=strategy)
  else if (class(data) == "VSLCMdataCategorical")
    reference <- new("VSLCMresultsCategorical", data=data, criteria=InitCriteria(), model=new("VSLCMmodel",g=g, omega=discrim), strategy=strategy)
  else if (class(data) == "VSLCMdataMixed")
    reference <- new("VSLCMresultsMixed", data=data, criteria=InitCriteria(), model=new("VSLCMmodel",g=g, omega=discrim), strategy=strategy)
  else
    stop()      
  return(reference)
}




###################################################################################
##' This function performs the variable selection and the maximum likelihood estimation of the Latent Class Model
##'
##' @param x data.frame. Rows correspond to observations and columns correspond to variables. Continuous variables must be "numeric", count variables must be "integer" and categorical variables must be "factor".
##' @param g numeric. It defines number of components.
##' @param vbleSelec logical. It indicates if a variable selection is done (1: yes, 0: no; default is 1).
##' @param crit.varsel character. It defines the information criterion used for the variable selection ("AIC", "BIC" or "MICL"; only used if vbleSelec=1; default is "BIC").
##' @param initModel numeric. It gives the number of initializations of the alternated algorithm maximizing the MICL criterion (only used if crit.varsel="MICL"; default is 50)
##' @param nbcores numeric.  It defines the numerber of cores used by the alogrithm (default is 1).
##' @param discrim numeric. It indicates if each variable is discrimiative (1) or irrelevant (0) (only used if vbleSelec=0; default is rep(1,ncol(x))).
##' @param nbSmall numeric. It indicates  the number of SmallEM algorithms performed for the ML inference (default is 250).
##' @param iterSmall numeric. It indicates  the number of iterations for each SmallEM algorithm (default is 20).
##' @param nbKeep numeric. It indicates the number of chains used for the final EM algorithm (default is 50).
##' @param iterKeep numeric. It indicates the maximal number of iterations for each EM algorithm (default is 1000).
##' @param tolKeep numeric. It indicates the maximal gap between two successive iterations of EM algorithm which stops the algorithm (default is 0.001).
##' 
##'  
##' @return Returns an instance of \linkS4class{VSLCMresultsMixed}.
##' @examples
##' \dontrun{
##' data(iris)
##' res.LCM <- VarSelCluster(x, 2)
##' summary(res.LCM)
##' @export
##'
##'
VarSelCluster <- function(x, g, vbleSelec=TRUE, crit.varsel="BIC", initModel=50,  nbcores=1, discrim=rep(1,ncol(x)), nbSmall=250, iterSmall=20,
                          nbKeep=50, iterKeep=1000, tolKeep=10**(-6)){
  reference <- BuildS4Reference(x, g, initModel, vbleSelec, crit.varsel, TRUE, nbcores, discrim, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  if (g==1){
    reference <- withoutmixture(reference)
  }else{
    # Estimation du modele et/ou des parametres
    if (strategy@parallel == FALSE)
      reference <- VarSelModelMLE(reference, 0)
    else{
      nb.cpus <- min(detectCores(all.tests = FALSE, logical = FALSE) , max(strategy@initModel,1), nbcores)
      if (strategy@crit.varsel == TRUE){
        reference@strategy <- JustModelStrategy(strategy, nb.cpus)
        
        if(Sys.info()["sysname"] == "Windows")
        {
          cl <- makeCluster(nb.cpus)
          common.objects <- c("reference","VarSelModelMLE", "OptimizeMICL")
          clusterEvalQ(cl, {require(VarSelLCM)})
          clusterExport(cl=cl, varlist = common.objects, envir = environment())
          reference <- parLapply(cl = cl, 
                                 X  = as.list(rep(0, nb.cpus)), 
                                 fun = function(g){VarSelModelMLE(reference,g)})
          stopCluster(cl)
          
        }
        else
          reference <- mclapply(X = as.list(rep(0, nb.cpus)),
                                FUN = VarSelModelMLE,
                                obj=reference,
                                mc.cores = nb.cpus, mc.preschedule = TRUE, mc.cleanup = TRUE)
        
        # On conserve le meilleur modele au sens de MICL
        tmpMICL <- rep(NA, length(reference))
        for (it in 1:length(reference)) tmpMICL[it] <- reference[[it]]@criteria@MICL
        
        cvrate <- 0
        for (it in which(tmpMICL==max(tmpMICL))){
          cvrate <- cvrate + reference[[it]]@criteria@cvrate
        }
        reference <- reference[[which.max(tmpMICL)]]
        reference@criteria@cvrate = cvrate
      }
      reference@strategy <- strategy 
      nb.cpus <- min(detectCores(all.tests = FALSE, logical = FALSE) , max(reference@strategy@nbSmall,1), nbcores, 4)
      if (strategy@paramEstim){
        reference@strategy@crit.varsel <- NULL
        reference@strategy@nbSmall <- ceiling(reference@strategy@nbSmall / nb.cpus)
        reference@strategy@nbKeep <- ceiling(reference@strategy@nbKeep / nb.cpus)
        
        if(Sys.info()["sysname"] == "Windows")
        {
          cl <- makeCluster(nb.cpus)
          common.objects <- c("reference","VarSelModelMLE", "OptimizeMICL")
          clusterEvalQ(cl, {require(VarSelLCM)})
          clusterExport(cl=cl, varlist = common.objects, envir = environment())
          reference <- parLapply(cl = cl, 
                                 X  = as.list(rep(0, nb.cpus)), 
                                 fun = function(g){VarSelModelMLE(reference,g)})
          stopCluster(cl)
          
        }
        else
          reference <- mclapply(X = as.list(rep(0, nb.cpus)), FUN = VarSelModelMLE, obj=reference, mc.cores = nb.cpus, mc.preschedule = TRUE, mc.cleanup = TRUE)
        
        # On conserve les parametres maximisant la vraisemblance
        tmploglike <- rep(NA, length(reference))
        for (it in 1:length(tmploglike)) {if (reference[[it]]@criteria@degeneracyrate!=1) tmploglike[it] <- reference[[it]]@criteria@loglikelihood}
        if (all(is.na(tmploglike))) tmploglike[1]=1
        reference <- reference[[which.max(tmploglike)]]
        reference@strategy <- strategy
      }
    }
  }
  return(DesignOutput(reference))
  #return(reference)
}