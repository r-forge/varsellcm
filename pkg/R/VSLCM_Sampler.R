VarSelSampler1 <- function(n, disc=3, nodisc=3, mudisc=2, sd=1){
    x <- matrix(0, n, disc + nodisc)
    
    n1 <- sum(sample(0:1, n, replace = TRUE))
    
    for (j in 1:disc){
      x[1:n1, j] <- rnorm(n1, mudisc, sd)
      x[(n1+1):n, j] <- rnorm(n - n1, -mudisc, sd)
    }
    for (j in 1:nodisc)
      x[, j+disc] <- rnorm(n, 0, sd)
    
    return(list(x=x, z=c(rep(1,n1), rep(2, n-n1))))
}