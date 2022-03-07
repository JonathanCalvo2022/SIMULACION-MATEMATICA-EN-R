v.a.exp <- function(n, lambda) {
  L <- -1/lambda
  u <- runif(n)
  x <- L * log(u)
}
############################DISTRIBUCION LAPLACE################################ 
v.a.laplace <- function(n, lambda,p){
  u <- runif(n)
  v <- runif(n)
  t <- -v.a.exp(n, lambda)
  if(v < p){
    x<- (u + t)
  }else{
    x<- (u - t)
  }
}
laplace<- v.a.laplace(n = 100, lambda = 2, p = 0.5)
hist(laplace)
mean(laplace)
################################################################################