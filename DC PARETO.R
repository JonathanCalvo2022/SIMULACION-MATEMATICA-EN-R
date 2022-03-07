set.seed(321)
# DISTRIBUCION PARETO
################################################################################
library(Pareto)
inv.pareto<-function(n,b,a){
  u<-runif(n)
  z<-(b/((1-u)^(1/a)));z
}
pareto <- inv.pareto(n = 100,b=5 ,a = 3)
################################################################################
hist(pareto, main = expression(f(x) == frac(gamma * k^gamma, x^(gamma + 1))),probability = T)
curve(dPareto(x,t= 5, alpha = 3),col="red",add = T) 
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(pareto, "pPareto",t =5,alpha =3)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################
muestra.teorica<- rPareto(n=100,t = 5,alpha = 3)
mean(muestra.teorica) # muestra real -------------------------------------------
mean(pareto) # muestra teorica -------------------------------------------------
################################################################################
