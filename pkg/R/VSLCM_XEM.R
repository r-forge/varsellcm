VSLCM_XEM <- function(x, z, g){
  n <- nrow(x)
  d <- ncol(x)
  pi <- table(c(1:g,z))/(n+g)
  mu <- matrix(colMeans(x), g, d,byrow = TRUE)
  va <- matrix(0, g, d)
  for (k in unique(z))
    mu[k,] <- colMeans(as.matrix(x[which(z==k),]))
  for (j in 1:d){
      va[,j] <- var(x[,j])
  }
  
  proba <- matrix(log(pi), n, g, byrow = TRUE)
  for (k in 1:g){
    for (j in 1:d){
      proba[,k] <- proba[,k] + dnorm(x[,j], mu[k,j], sqrt(va[k,j]),log=TRUE)
    }
  }
  maxcol <- proba[,1]
  for (k in 2:g){
    who <- which(maxcol < proba[,k])
    maxcol[who] <- proba[who, k]
  }
  
  prec <- -Inf
  loglike <- sum(maxcol) + sum(log( rowSums(exp(sweep(proba, 1, maxcol, "-")))))
  
  while (loglike - prec > 0.001){
    proba <- exp(sweep(proba, 1, maxcol, "-"))
    tik <- proba/rowSums(proba)
    
    pi <- colSums(tik)/n
    for (k in 1:g){
      for (j in 1:d){
        mu[k,j] <- sum(x[,j]*tik[,k])/sum(tik[,k])
        va[k,j] <- sum(tik[,k] * ((x[,j] - mu[k,j])**2)) / sum(tik[,k])
      }
    }
   
    proba <- matrix(log(pi), n, g, byrow = TRUE)
    for (k in 1:g){
      for (j in 1:d){
        proba[,k] <- proba[,k] + dnorm(x[,j], mu[k,j], sqrt(va[k,j]),log=TRUE)
      }
    }
    maxcol <- proba[,1]
    for (k in 2:g){
      who <- which(maxcol < proba[,k])
      maxcol[who] <- proba[who, k]
    }
    
    
    prec <- loglike
    loglike <- sum(maxcol) + sum(log( rowSums(exp(sweep(proba, 1, maxcol, "-")))))
  }
  z <- rep(0,n)
  for (k in 1:g){
    z[which(rowSums( sweep(tik,1,tik[,k],"<=") ) == g)] <- k
  }
    
  pen <- ((g-1) + 2*g*d)*0.5*log(n)
  
  return(list(means=mu, variances=va, loglike=loglike, bic=loglike - pen, tik=tik, pi=pi, z=z))
}