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


varsellcm.plot.cate  <- function(tmp, y){
  
  df <- data.frame(class=as.factor(rep(1:nrow(tmp), ncol(tmp))),
                   levels=rep(colnames(tmp), each=nrow(tmp)),
                   probs=round(as.numeric(tmp), 2))
  
  
  
  graph <- ggplot(data=df, aes(x=levels, y=probs, fill=class)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=probs), vjust=1.6, color="black",  position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle(paste("Distribution per class of", y)) +  
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  print(graph)
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
  signature = c("VSLCMresultsCategorical", "character"),
  definition = function(x, y)varsellcm.plot.cate(x@param@alpha[[y]], y)
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
    }else if (x@data@withCategorical){
      if (y %in% names(x@param@paramCategorical@alpha)){
        loc2 <- which(names(x@param@paramCategorical@alpha) ==y)
        ifelse (length(loc2)==1,
                varsellcm.plot.cate(x@param@paramCategorical@alpha[[loc2]], y),
                stop("y must be the name of a variable in the analyzed data"))
        
        vu <- TRUE
      }
    }
    
    if (!vu) stop("y must be the name of a variable in the analyzed data")
    
    
    
  }
)

