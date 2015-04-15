plotCateg <- function(data, model, param){
  if (sum(model@omega)>0){
    results <- matrix(0,model@g,data@d)
    for (j in 1:data@d){
      for (k in 1:model@g){
        alpha <- 0
        for (k2 in (c(1:model@g)[-k]))
          alpha <- alpha + (param@pi[k2]* param@alpha[[j]][k2,])/sum(param@pi[-k])
        
        results[k,j] <- round(sum((param@alpha[[j]][k,]-alpha)**2),4)
      }
    }
    if (any(model@omega==0))  
      results <- results[, -which(model@omega==0)]
    
    mp<-barplot(results, beside = TRUE, axisnames = FALSE, names.arg=colnames(data@shortdata)[which(model@omega==1)], main="Discriminative categorical variables", ylab="Discriminative measure", ylim=c(0,max(results)), cex.main=0.8)
    mtext(1, at = mp, text = paste("C",1:model@g), line = 0, cex = 0.5)
    mtext(1, at = colMeans(mp), text = colnames(data@shortdata)[which(model@omega==1)], line = 2) 
  }else
    plot(NA, xlim=c(0,1), ylim=c(0,1),xlab = "", ylab="", axes = F, main="Discriminative categorical variables", cex.main=0.8)
}

setMethod(
  f="plot",
  signature = c("VSLCMresultsCategorical"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1))
      plotCateg(x@data, x@model, x@param)
    else
      cat("No plot is available since none variable is discriminative!")
  }
)

plotCont <- function(data, model, param){
  if (sum(model@omega)>0){
    results <- matrix(0,model@g,data@d)
    for (j in 1:data@d){
      canddef <- range(data@data[,j], na.rm = T)
      canddef <- canddef + c(-0.05,0.05)*(canddef[2] - canddef[1])
      for (k in 1:model@g){
        ftmp <- function(u){
          out <- dnorm(u, param@mu[j,k], param@sd[j,k])
          for (k2 in(c(1:model@g)[-k])) out <- out - param@pi[k2]/sum(param@pi[-k]) * dnorm(u, param@mu[j,k2], param@sd[j,k2])
          return(out**2)
        }
        results[k,j] <- round(integrate(ftmp, lower = canddef[1], upper = canddef[2], subdivisions = 10**5)$value,4)
      }
    }
    if (any(model@omega==0))  
      results <- results[, -which(model@omega==0)]
    
    mp<-barplot(results, beside = TRUE, axisnames = FALSE, names.arg=colnames(data@data)[which(model@omega==1)], main="Discriminative continuous variables", ylab="Discriminative measure", ylim=c(0,max(results)), cex.main=0.8)
    mtext(1, at = mp, text = paste("C",1:model@g), line = 0, cex = 0.5)
    mtext(1, at = colMeans(mp), text = colnames(data@data)[which(model@omega==1)], line = 2)
  }else
    plot(NA, xlim=c(0,1), ylim=c(0,1),xlab = "", ylab="", axes = F, main="Discriminative continuous variables", cex.main=0.8)
  
}

setMethod(
  f="plot",
  signature = c("VSLCMresultsContinuous"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1))
      plotCont(x@data, x@model, x@param)    
    else
      cat("No plot is available since none variable is discriminative!")
  }
)


setMethod(
  f="plot",
  signature = c("VSLCMresultsMixed"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1)){
      par(mfrow=c(1,x@data@withContinuous+x@data@withCategorical))
      if (x@data@withContinuous)
        plotCont(x@data@dataContinuous, new("VSLCMmodel", g=x@model@g, omega=x@model@omega[which(names(x@model@omega) %in% colnames(x@data@dataContinuous@data))]), x@param@paramContinuous)    
      
      if (x@data@withCategorical)
        plotCateg(x@data@dataCategorical, new("VSLCMmodel", g=x@model@g, omega=x@model@omega[which(names(x@model@omega) %in% colnames(x@data@dataCategorical@shortdata))]), x@param@paramCategorical)    
      
    }
  }
)

