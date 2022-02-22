set.seed(789)
################################################################################
#Distribucion Geométrica
v.a.g<- function(n,lamda=p){
  normal<-runif(n)
  digeo<-c()
  if(lamda > 0){
    digeo<- (-log(1 - normal)) / lamda
  }
}
g <- v.a.g(n=10^4, lamda= 0.7)
mean(g)
hist(g, main = "Distribucion Geometrica",probability = T)
curve(dgeom(x,prob = 0.7),col="red",add = T) 
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(g, "pgeom",prob=0.7)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################
#Distribución Binomial Negativa
v.a.bn <- function(n,r,p){
  bn <- replicate(r,v.a.g(10^4,0.7))
  bn <- rowSums(bn)
}
set.seed(789)
bn<- v.a.bn(10^4,r = 10,p = 0.7)
mean(bn)
hist(bn, main = "Distribucion Binomial Negativa",probability = T)
#y <- seq(min(bn),max(bn),0.1)
#lines(y,mandar la formula,col="red")
################################################################################
