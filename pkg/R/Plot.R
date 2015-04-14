setMethod(
  f="plot",
  signature = c("VSLCMresultsCategorical"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1)){
      results <- matrix(0,x@model@g,x@data@d)
      for (j in 1:x@data@d){
        for (k in 1:x@model@g){
          alpha <- 0
          for (k2 in (c(1:x@model@g)[-k]))
            alpha <- alpha + (x@param@pi[k2]* x@param@alpha[[j]][k2,])/sum(x@param@pi[-k])
          
          results[k,j] <- round(mean((x@param@alpha[[j]][k,]-alpha)**2),4)
        }
      }
      if (any(x@model@omega==0))  
        results <- results[, -which(x@model@omega==0)]
      
      mp<-barplot(results, beside = TRUE, axisnames = FALSE, names.arg=colnames(x@data@shortdata)[which(x@model@omega==1)], main="", ylab="Discriminative measure", ylim=c(0,max(results)))
      mtext(1, at = mp, text = paste("C",1:x@model@g), line = 0, cex = 0.5)
      mtext(1, at = colMeans(mp), text = colnames(x@data@shortdata)[which(x@model@omega==1)], line = 2) 
    }else{
      cat("No plot is available since none variable is discriminative!")
    }
    
  }
)

setMethod(
  f="plot",
  signature = c("VSLCMresultsContinuous"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1)){
      results <- matrix(0,x@model@g,x@data@d)
      for (j in 1:x@data@d){
        canddef <- range(x@data@data[,j], na.rm = T)
        canddef <- canddef + c(-0.05,0.05)*(canddef[2] - canddef[1])
        for (k in 1:x@model@g){
          ftmp <- function(u){
            out <- dnorm(u, x@param@mu[j,k], x@param@sd[j,k])
            for (k2 in(c(1:x@model@g)[-k])) out <- out - x@param@pi[k2]/sum(x@param@pi[-k]) * dnorm(u, x@param@mu[j,k2], x@param@sd[j,k2])
            return(out**2)
          }
          results[k,j] <- round(integrate(ftmp, lower = canddef[1], upper = canddef[2], subdivisions = 10**5)$value,4)
        }
      }
      if (any(x@model@omega==0))  
        results <- results[, -which(x@model@omega==0)]
      
      mp<-barplot(results, beside = TRUE, axisnames = FALSE, names.arg=colnames(x@data@data)[which(x@model@omega==1)], main="", ylab="Discriminative measure", ylim=c(0,max(results)))
      mtext(1, at = mp, text = paste("C",1:x@model@g), line = 0, cex = 0.5)
      mtext(1, at = colMeans(mp), text = colnames(x@data@data)[which(x@model@omega==1)], line = 2) 
      
    }else{
      cat("No plot is available since none variable is discriminative!")
    }
  }
)


setMethod(
  f="plot",
  signature = c("VSLCMresultsMixed"),
  definition = function(x){
    if (any(x@model@omega==1) && (x@model@g>1)){
      results <- matrix(0,x@model@g,x@data@d)
      loc <- 0
      if (x@data@withContinuous){
        for (j in 1:x@data@dataContinuous@d){
          loc <- loc + 1
          canddef <- range(x@data@dataContinuous@data[,j], na.rm = T)
          canddef <- canddef + c(-0.05,0.05)*(canddef[2] - canddef[1])
          for (k in 1:x@model@g){
            ftmp <- function(u){
              out <- dnorm(u, x@param@paramContinuous@mu[j,k], x@param@paramContinuous@sd[j,k])
              for (k2 in(c(1:x@model@g)[-k])) out <- out - x@param@pi[k2]/sum(x@param@pi[-k]) * dnorm(u, x@param@paramContinuous@mu[j,k2], x@param@paramContinuous@sd[j,k2])
              return(out**2)
            }
            results[k,loc] <- round(integrate(ftmp, lower = canddef[1], upper = canddef[2], subdivisions = 10**5)$value,4)
          }
        }
      }
      
      if (x@data@withCategorical){
        for (j in 1:x@data@dataCategorical@d){
          loc <- loc + 1
          for (k in 1:x@model@g){
            alpha <- 0
            for (k2 in (c(1:x@model@g)[-k]))
              alpha <- alpha + (x@param@pi[k2]* x@param@paramCategorical@alpha[[j]][k2,])/sum(x@param@pi[-k])
            results[k,loc] <- round(mean((x@param@paramCategorical@alpha[[j]][k,]-alpha)**2),4)
          }
        }
        
      }
      if (any(x@model@omega==0))  
        results <- results[, -which(x@model@omega==0)]
      
      mp<-barplot(results, beside = TRUE, axisnames = FALSE, names.arg= names(x@model@omega)[which(x@model@omega==1)], main="", ylab="Discriminative measure", ylim=c(0,max(results)))
      mtext(1, at = mp, text = paste("C",1:x@model@g), line = 0, cex = 0.5)
      mtext(1, at = colMeans(mp), text = names(x@model@omega)[which(x@model@omega==1)], line = 2) 
    }else{
      cat("No plot is available since none variable is discriminative!")
    }
  }
)
