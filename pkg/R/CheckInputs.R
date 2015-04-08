# Verifie les paramètres d'entrées
CheckInputs <- function(x, g, initModel, vbleSelec, paramEstim, parallel, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep){
  if ( (is.numeric(g)==FALSE) || (length(g)!=1))
    stop("The component number have to be an integer of length one!")
  
  if (is.data.frame(x)==FALSE)
    stop("Data set must be a data frame!")
  
  if (is.logical(vbleSelec) == FALSE)
    stop("Input vbleSelec must be logicial")
  
  if (is.logical(paramEstim) == FALSE)
    stop("Input paramEstim must be logicial")
  
  if (is.logical(parallel) == FALSE)
    stop("Input parallel must be logicial")
  
  if ((is.numeric(nbSmall) == FALSE) || (length(nbSmall)!=1))
    stop("Input nbSmall must be numeric of size one")
  
  if ((is.numeric(iterSmall) == FALSE) || (length(iterSmall)!=1))
    stop("Input iterSmall must be numeric of size one")
  
  if ((is.numeric(nbKeep) == FALSE) || (length(nbKeep)!=1))
    stop("Input nbKeep must be numeric of size one")
  
  if ((is.numeric(iterKeep) == FALSE) || (length(iterKeep)!=1))
    stop("Input iterKeep must be numeric of size one")
  
  if ((is.numeric(tolKeep) == FALSE) || (length(tolKeep)!=1))
    stop("Input tolKeep must be numeric of size one")  
  

}