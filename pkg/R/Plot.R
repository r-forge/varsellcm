cdfmixtureGauss <- function(u, pi, mu, sd){
  out <- u * 0
  for (k in 1:length(pi)) 
    out <- out + pnorm(u, mu[k], sd[k])*pi[k]
  out
}

varsellcm.plot.cont.cdf <- function(df, y, param){
  graphic <- ggplot(df, aes(x=x)) +   
    stat_ecdf(geom = "step", aes(colour="Empirical"), size=1) +
    stat_function(fun = cdfmixtureGauss, args = param, aes(colour = "Theoretical"), size=1) +
    scale_colour_manual("CDF", values = c("black", "dodgerblue3")) +
    scale_x_continuous(name = y) + 
    scale_y_continuous(name="CDF") + 
    ggtitle(paste("CDF of", y)) +  
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  print(graphic)
}

varsellcm.plot.boxplot <- function(df, y){
  graphic <- ggplot(df, aes(x=class, y=x, fill=class)) + 
    geom_boxplot() +   
    guides(fill=FALSE) +
    coord_flip() +
    scale_y_continuous(name=y)  + 
    ggtitle(paste("Boxplots of", y)) +  
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  print(graphic)
}


#' @rdname plot-methods
#' @aliases plot plot, VSLCMresultsContinuous-method
setMethod(
  f="plot",
  signature = c("VSLCMresultsContinuous", "character"),
  definition = function(x, y, type){
    loc2 <- which(rownames(x@param@mu)==y)
    if (length(loc2)!=1)
      stop("y must be the name of a variable in the analyzed data")
    if (type=="cdf")
      varsellcm.plot.cont.cdf(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)]),
                              y, 
                              list(x@param@pi, x@param@mu[loc2,], x@param@sd[loc2,]))
    
    else if (type=="boxplot") 
      varsellcm.plot.boxplot(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)], 
                                             class=as.factor(x@partitions@zMAP)),
                                  y)
    
    else
      stop("type must be cdf or boxplot")
  }
)

cdfmixturePoiss <- function(u, pi, lam){
  out <- u * 0
  for (k in 1:length(pi)) 
    out <- out + pnorm(u, lam[k])*pi[k]
  out
}

varsellcm.plot.inte.cdf <- function(df, y, param){
  graphic <- ggplot(df, aes(x=x)) +   
    stat_ecdf(geom = "step", aes(colour="Empirical"), size=1) +
    stat_function(fun = cdfmixturePoiss, args = param, aes(colour = "Theoretical"), size=1) +
    scale_colour_manual("CDF", values = c("black", "dodgerblue3")) +
    scale_x_continuous(name = y) + 
    scale_y_continuous(name="CDF") + 
    ggtitle(paste("CDF of", y)) +  
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  print(graphic)
}


#' @rdname plot-methods
#' @aliases plot plot, VSLCMresultsInteger-method
setMethod(
  f="plot",
  signature = c("VSLCMresultsInteger", "character"),
  definition = function(x, y, type){
    loc2 <- which(rownames(x@param@lambda)==y)
    if (length(loc2)!=1)
      stop("y must be the name of a variable in the analyzed data")
    if (type=="cdf")
      varsellcm.plot.inte.cdf(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)]),
                              y, 
                              list(x@param@pi, x@param@lambda[loc2,]))
    
    else if (type=="boxplot") 
      varsellcm.plot.boxplot(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)], 
                                             class=as.factor(x@partitions@zMAP)),
                                  y)
    
    else
      stop("type must be cdf or boxplot")
  }
)


plotCateg <- function(data, model, param){
  
  results <- matrix(0,model@g,data@d)
  for (j in 1:data@d){
    for (k in 1:model@g){
      alpha <- 0
      for (k2 in (c(1:model@g)[-k]))
        alpha <- alpha + (param@pi[k2]* param@alpha[[j]][k2,])/sum(param@pi[-k])
      
      results[k,j] <- round(sum((param@alpha[[j]][k,]-alpha)**2),4)
    }
  }
  
  for (j in  which(model@omega==1)){
    mp<-barplot(matrix(results[,j], ncol = 1), beside = TRUE, axisnames = FALSE, main=(colnames(data@shortdata))[j],  ylab="Discriminative measure", ylim=c(0,max(c(results,1))), cex.main=0.8)
    mtext(1, at = mp, text = paste("C",1:model@g), line = 0, cex = 0.5)
  }
}
#' 
#' This function draws information about an instance of \code{\linkS4class{VSLCMresultsContinuous}}, \code{\linkS4class{VSLCMresultsInteger}}, \code{\linkS4class{VSLCMresultsCategorical}} or \code{\linkS4class{VSLCMresultsMixed}}.
#' 
#' @param object instance of  \code{\linkS4class{VSLCMresultsContinuous}}, \code{\linkS4class{VSLCMresultsInteger}}, \code{\linkS4class{VSLCMresultsCategorical}} or \code{\linkS4class{VSLCMresultsMixed}}..
#' 
#' @name plot
#' @rdname plot-methods
#' @docType methods
#' @exportMethod plot
## Surcharge pour VSLCMresultsCategorical
#' @rdname plot-methods
#' @aliases plot plot,VSLCMresultsCategorical-method
setMethod(
  f="plot",
  signature = c("VSLCMresultsCategorical"),
  definition = function(x){
    op <- par(no.readonly = TRUE)
    if (any(x@model@omega==1) && (x@model@g>1)){
      nvar <- sum(x@model@omega)
      # split the layout
      if( nvar < 4 & nvar > 1){
        par( mfrow = c( 1, nvar ), mar=c(2,4,2,2), cex.axis = 0.6, cex.lab = 0.6, cex.main = 0.7  )
      }else if ( nvar >= 4 ){
        nrow<-round(sqrt(nvar))
        ncol <- ceiling(nvar/nrow)
        par( mfrow = c( nrow, ncol ), mar=c(2,4,2,2), cex.axis = 0.6, cex.lab = 0.6, cex.main = 0.7  ) 
      }
      plotCateg(x@data, x@model, x@param)
    }
    else
      cat("No plot is available since none variable is discriminative!")
    par(op)
  }
  
)



## Surcharge pour VSLCMresultsMixed
#' @rdname plot-methods
#' @aliases plot plot,VSLCMresultsMixed-method
setMethod(
  f="plot",
  signature = c("VSLCMresultsMixed", "character"),
  definition = function(x, y, type){
    vu <- FALSE
    if (x@data@withContinuous){
      if (y %in% rownames(x@param@paramContinuous@mu)){
        loc2 <- which(rownames(x@param@paramContinuous@mu)==y)
        if (length(loc2)!=1)
          stop("y must be the name of a variable in the analyzed data")
        if (type=="cdf")
          varsellcm.plot.cont.cdf(data.frame(x = x@data@dataContinuous@data[, which(rownames(x@param@paramContinuous@mu)==y)]),
                                  y, 
                                  list(x@param@pi, x@param@paramContinuous@mu[loc2,], x@param@paramContinuous@sd[loc2,]))
        else if (type=="boxplot") 
          varsellcm.plot.boxplot(data.frame(x = x@data@dataContinuous@data[, which(rownames(x@param@paramContinuous@mu)==y)], 
                                                 class=as.factor(x@partitions@zMAP)),
                                      y)
        else
          stop("type must be cdf or boxplot")
        vu <- TRUE
      }
    }else if (x@data@withInteger){
      if (y %in% rownames(x@param@paramInteger@lambda)){
        loc2 <- which(rownames(x@param@paramInteger@lambda)==y)
        if (length(loc2)!=1)
          stop("y must be the name of a variable in the analyzed data")
        if (type=="cdf")
          varsellcm.plot.inte.cdf(data.frame(x = x@data@dataInteger@data[, which(rownames(x@param@paramInteger@lambda)==y)]),
                                  y, 
                                  list(x@param@pi, x@param@paramInteger@lambda[loc2,]))
        else if (type=="boxplot") 
          varsellcm.plot.boxplot(data.frame(x = x@data@dataInteger@data[, which(rownames(x@param@paramInteger@lambda)==y)], 
                                            class=as.factor(x@partitions@zMAP)),
                                 y)
        else
          stop("type must be cdf or boxplot")
        vu <- TRUE
      }
    }
    
    if (!vu) stop("y must be the name of a variable in the analyzed data")
    
    
    
  }
)

