set.seed(321)
#1) DISTRIBUCION EXPONENCIAL
inv.exp <- function(n, lambda){
  u <- runif(n)
  -log(1 - u) / lambda
}
#4) DISTRIBUCION GAMMA(ALPHA,BETA) CON EXPONENCIAL
################################################################################
# PRIMERA FORMA
inv.gamma1<- function(n,r,lambda){
  m <- r*n
  exp <- inv.exp(m,lambda)
  dim(exp) <- c(n,r)
  gamma <- rowSums(exp,na.rm = T);gamma
}
gama1<- inv.gamma1(n=10^6,r=3,lambda = 1/7)
################################################################################
# SEGUNDA FORMA
inv.gamma2<- function(n, shape,es){
  mx <-matrix(NA,n,shape)
  i<-1
  while(i<=shape){
  mx[,i]<-inv.exp(n, 1/es)
  i<-i+1
  }
  rowSums(mx)
}
gama2<-inv.gamma2(10^6,3,7)
################################################################################
# GRAFICA DE AMBOS CASOS
par(mfrow = c(1, 2)) 
#Primera forma gamma1
hist(gama1, main = "DISTRIBUCION GAMMA1",probability = T) 
curve(dgamma(x,shape =3,scale=7),col="blue",add = T)
#Segunda forma gamma2
hist(gama2, main = "DISTRIBUCION GAMMA2",probability = T) 
curve(dgamma(x,shape =3,scale=7),col="red",add = T)
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(gama1, "pgamma", shape =3,scale=7)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################