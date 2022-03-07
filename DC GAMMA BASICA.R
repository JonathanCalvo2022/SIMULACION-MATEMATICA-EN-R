####################################DISTRIBUCION GAMA###########################
gamma.dis<- function(m, n, lambda) {
  g <- rep(0,m)
  for(i in 1:m) {
    u <- runif(n);
    g <- sum(log(u))/lambda
  }
  return(g)
}
m<- gamma.dis(m= 100, n=7, lambda = 4)
m
################################################################################