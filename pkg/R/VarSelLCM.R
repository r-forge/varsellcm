########################################################################################################################
## Fonctions principales du package, la seule accessible par l'utilisateur est VarSelCluster
########################################################################################################################

# 
# ########################################################################################################################
# ## La fonction VarSelModelSelection permet d'effectuer le choix de model et l'estimation des paramètres en appeleant le
# ## code C++. Il retourne un objet VSLCMresultsContinuous ou VSLCMresultsCategorical en fonction de la nature des données.
# ########################################################################################################################
# setGeneric ( name= "VarSelModelSelection",  def = function(data, g, strategy){ standardGeneric("VarSelModelSelection")})
# ## Pour les variables continues
# setMethod( f = "VarSelModelSelection", 
#            signature(data="VSLCMdataContinuous", g="numeric", strategy="VSLCMstrategy"), 
#            definition = function(data, g, strategy){
#              reference <- new("VSLCMresultsContinuous", data=data, criteria=new("VSLCMcriteria", MICL=-Inf), model=new("VSLCMmodel",g=g, omega=rep(1, data@d)), strategy=strategy)
#              reference <- OptimizeMICL(reference, "Continuous")
#              return(DesignOutput(reference))             
#            }
# )
# ## Pour les variables catégorielles
# setMethod( f = "VarSelModelSelection", 
#            signature(data="VSLCMdataCategorical", g="numeric", strategy="VSLCMstrategy"), 
#            definition = function(data, g, strategy){
#              reference <- new("VSLCMresultsCategorical", data=data, criteria=new("VSLCMcriteria", MICL=-Inf), model=new("VSLCMmodel",g=g, omega=rep(1, data@d)), strategy=strategy)
#              reference <- OptimizeMICL(reference, "Categorical")
#              return(DesignOutput(reference))             
#            }
# )


########################################################################################################################
## La fonction VarSelModelMLE permet d'effectuer l'estimation des paramètres en considèrant que les variables données
## dans le slot model de l'objet VSLCMresultsContinuous ou VSLCMresultsCategorical.
## Il appelle le code c++ et retourne un objet VSLCMresultsContinuous ou VSLCMresultsCategorical en fonction de la
## nature des données.
########################################################################################################################
setGeneric ( name= "VarSelModelMLE",  def = function(obj,it){ standardGeneric("VarSelModelMLE")})
## Pour les variables continues
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsContinuous",it="numeric"), 
           definition = function(obj,it){
             reference <- OptimizeMICL(obj, "Continuous")
             return(reference)         
           }
)
## Pour les variables catégorielles
setMethod( f = "VarSelModelMLE", 
           signature(obj="VSLCMresultsCategorical",it="numeric"), 
           definition = function(obj, it){
             reference <- OptimizeMICL(obj, "Categorical")
             return(reference)           
           }
)


########################################################################################################################
## La fonction VarSelCluster est disponible pour l'utilisateur et permet d'appeller les fonctions VarSelModelSelection et
## VarSelModelMLE et retourne un objet VSLCMresultsContinuous ou VSLCMresultsCategorical
## La fonction possède deux paramètres obligatoires:
## x: tableau de données sous format data.frame avec variables numeric pour les continues et factor pour les categorielles
## g: nombre de classes (numeric de taille 1)
## La fonction possède également 9 paramètres optionnels
## initModel: nomber d'initialisations de l'algorithme d'optimisation de MICL
## vbleSelec: logical indiquant si la sélection de variables est effectuée
## paramEstim: logical indiquant si l'estimation des paramètres est effectuée
## parallel: logical indiquant si le code est parallèlisé
## nbSmall: nombre d'initialisations du small EM
## iterSmall: nombre d'itérations des small EM
## nbKeep: nombre de chaines conservées après le small EM
## iterKeep: nombre d'itérations maximum des EM
## tolKeep: difference des vraisemblances de deux iterations successives impliquant un arret de EM
########################################################################################################################
VarSelCluster <- function(x, g, initModel=50, vbleSelec=TRUE, paramEstim=TRUE, parallel=FALSE, nbSmall=250, iterSmall=20, nbKeep=50, iterKeep=10**3, tolKeep=10**(-3)){
  # Verifie les paramètres d'entrées
  CheckInputs(x, g, initModel, vbleSelec, paramEstim, parallel, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  # Création de l'objet S4 VSLCMstrategy contenant les paramètres de réglage
  strategy <- VSLCMstrategy(initModel, parallel, vbleSelec, paramEstim, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)  
  # Création de l'objet S4 VSLCMdataContinuous ou VSLCMdataCategorical
  data <- VSLCMdata(x)
  
  if (class(data) == "VSLCMdataContinuous")
    reference <- new("VSLCMresultsContinuous", data=data, criteria=new("VSLCMcriteria", MICL=-Inf), model=new("VSLCMmodel",g=g, omega=rep(1, data@d)), strategy=strategy)
  else if (class(data) == "VSLCMdataCategorical")
    reference <- new("VSLCMresultsCategorical", data=data, criteria=new("VSLCMcriteria", MICL=-Inf), model=new("VSLCMmodel",g=g, omega=rep(1, data@d)), strategy=strategy)
  else
    stop()      
  
  # Estimation du modèle et/ou des paramètres
  if (strategy@parallel == FALSE)
    reference <- VarSelModelMLE(reference, 0)
  else{
    nb.cpus <- min(detectCores(all.tests = FALSE, logical = FALSE) , max(strategy@initModel,1))
    if (strategy@vbleSelec == TRUE){
      reference@strategy <- JustModelStrategy(strategy, nb.cpus)
      reference <- mclapply(X = as.list(rep(0, nb.cpus)),
                            FUN = VarSelModelMLE,
                            obj=reference,
                            mc.cores = nb.cpus, mc.preschedule = TRUE, mc.cleanup = TRUE)
      # On conserve le meilleur modèle au sens de MICL

      tmpMICL <- rep(NA, length(reference))
      for (it in 1:length(reference)) tmpMICL[it] <- reference[[it]]@criteria@MICL
      reference <- reference[[which.max(tmpMICL)]]
      # On parallelise aussi pour les EM donc on réparti les initialisations sur les différents coeurs
    }
    reference@strategy <- strategy 
    nb.cpus <- min(detectCores(all.tests = FALSE, logical = FALSE) , max(reference@strategy@nbSmall,1))
    if (strategy@paramEstim){
      reference@strategy@vbleSelec <- FALSE
      reference@strategy@nbSmall <- ceiling(reference@strategy@nbSmall / nb.cpus)
      reference@strategy@nbKeep <- ceiling(reference@strategy@nbKeep / nb.cpus)
      reference <- mclapply(X = as.list(rep(0, nb.cpus)), FUN = VarSelModelMLE, obj=reference, mc.cores = nb.cpus, mc.preschedule = TRUE, mc.cleanup = TRUE)
      # On conserve les paramètres maximisant la vraisemblance
      tmploglike <- rep(NA, length(reference))
      for (it in 1:length(tmploglike)) tmploglike[it] <- reference[[it]]@criteria@loglikelihood
      reference <- reference[[which.max(tmploglike)]]
      reference@strategy <- strategy
    }
  }
  return(DesignOutput(reference))
}