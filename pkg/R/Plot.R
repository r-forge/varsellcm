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


# setMethod(
#   f="plot",
#   signature = c("VSLCMresultsContinuous", "character"),
#   definition = function(x, y, type){
#     loc2 <- which(rownames(x@param@mu)==y)
#     if (length(loc2)!=1)
#       stop("y must be the name of a variable in the analyzed data")
#     if (type=="cdf")
#       varsellcm.plot.cont.cdf(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)]),
#                               y, 
#                               list(x@param@pi, x@param@mu[loc2,], x@param@sd[loc2,]))
#     
#     else if (type=="boxplot") 
#       varsellcm.plot.boxplot(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)], 
#                                         class=as.factor(x@partitions@zMAP)),
#                              y)
#     
#     else
#       stop("type must be cdf or boxplot")
#   }
# )

cdfmixturePoiss <- function(u, pi, lam){
  out <- u * 0
  for (k in 1:length(pi)) 
    out <- out + ppois(u, lam[k])*pi[k]
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


# setMethod(
#   f="plot",
#   signature = c("VSLCMresultsInteger", "character"),
#   definition = function(x, y, type){
#     loc2 <- which(rownames(x@param@lambda)==y)
#     if (length(loc2)!=1)
#       stop("y must be the name of a variable in the analyzed data")
#     if (type=="cdf")
#       varsellcm.plot.inte.cdf(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)]),
#                               y, 
#                               list(x@param@pi, x@param@lambda[loc2,]))
#     
#     else if (type=="boxplot") 
#       varsellcm.plot.boxplot(data.frame(x = x@data@data[, which(colnames(x@data@data)==y)], 
#                                         class=as.factor(x@partitions@zMAP)),
#                              y)
#     
#     else
#       stop("type must be cdf or boxplot")
#   }
# )


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
# setMethod(
#   f="plot",
#   signature = c("VSLCMresultsCategorical", "character"),
#   definition = function(x, y)varsellcm.plot.cate(x@param@alpha[[y]], y)
# )


#' 
#' This function draws information about an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of  \code{\linkS4class{VSLCMresults}}.
#' 
#' @name plot
#' @rdname plot-methods
#' @docType methods
#' @exportMethod plot
#' @rdname plot-methods
#' @aliases plot plot,VSLCMresults-method
setMethod(f="plot",
  signature = c("VSLCMresults", "character"),
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
    }
    if (x@data@withInteger){
      if (y %in% rownames(x@param@paramInteger@lambda)){
        loc2 <- which(rownames(x@param@paramInteger@lambda)==y)
        if (length(loc2)!=1)
          stop("y must be the name of a variable in the analyzed datazzz")
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
    if (x@data@withCategorical){
      if (y %in% names(x@param@paramCategorical@alpha)){
        loc2 <- which(names(x@param@paramCategorical@alpha) ==y)
        ifelse (length(loc2)==1,
                varsellcm.plot.cate(x@param@paramCategorical@alpha[[loc2]], y),
                stop("y must be the name of a variable in the analyzed data"))
        
        vu <- TRUE
      }
    }
    if (!vu) 
      stop("y must be the name of a variable in the analyzed data")
  }
) 



#' 
#' This function draws information about an instance of \code{\linkS4class{VSLCMresults}}.
#' 
#' @param object instance of  \code{\linkS4class{VSLCMresults}}.
#' 
#' @name plot
#' @rdname plot-methods
#' @docType methods
#' @exportMethod plot
#' @rdname plot-methods
#' @aliases plot plot,VSLCMresults-method
setMethod(
  f="plot",
  signature = c("VSLCMresults"),
  definition = function(x, type){
    print("la")
    df <- data.frame(discrim.power=obj@criteria@discrim, variables=as.factor(names(obj@criteria@discrim)), rg=1:obj@data@d)
    df <- df[which(df$discrim.power>0),]
    if (type=="pie"){
      pie<- ggplot(df, aes(x="", y=discrim.power, fill=variables))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0)  + 
        ggtitle(paste("Discriminative power")) +  
        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
      print(pie)
    }else if (type=="bar"){
      bar <- ggplot(data=df, aes(x=rg, y=discrim.power, fill=variables)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=round(discrim.power,2)), vjust=-0.1, color="black",
                  position = position_dodge(0.9), size=3.5)+
        scale_x_discrete(name="Variables")+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()  + 
        ggtitle(paste("Discriminative power")) +  
        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
      print(bar)
    }else{
      stop("type must be specified and equal to pie or bar")
    }
  }
)

