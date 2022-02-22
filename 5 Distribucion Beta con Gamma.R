set.seed(321)
################################################################################
#1) DISTRIBUCION EXPONENCIAL
inv.exp <- function(n, lambda){
  u <- runif(n)
  -log(1 - u) / lambda
}
################################################################################
#4) DISTRIBUCION GAMMA(ALPHA,BETA) CON EXPONENCIAL
inv.gamma <- function(n,r,lambda){
  m <- r*n
  exp <- inv.exp(m,lambda)
  dim(exp) <- c(n,r)
  gamma <- rowSums(exp,na.rm = T);gamma
}
################################################################################
#5) DISTIRBUCION BETA A PARTIR DE GAMMA
inv.betta <-function(n,a,b){
  k1<-inv.gamma(n,r = a, lambda = 1)
  k2<-inv.gamma(n,r = b, lambda = 1)
  k1/(k1+k2)
}
beta<- inv.betta(n= 10^6 , a = 5,b = 3)
hist(beta, main = "DISTRIBUCION BETA(5, 3)",probability = T)
curve(dbeta(x,shape1 = 5,shape2 = 3),col="red",add = T)
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(beta, "pbeta",shape1 = 5,shape2 = 3)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################
