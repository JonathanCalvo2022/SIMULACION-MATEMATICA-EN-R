#7) DISTRIBUCION CAUCHY CON SU INVERSA
################################################################################
inv.cauchy<-function(n){
  u<-runif(n)
  x <- tan(pi*u)
  x
  x <- x[x>= -5 & x<=5]
}
cauchy<-inv.cauchy(n=10^6)
################################################################################
hist(cauchy, prob = TRUE, main = expression(f(x) == 1/(pi * (1 + x^2)) ) ) 
y <- seq(-5, 5, 0.01)
lines(y, 1/(pi * (1 + y^2)),col = "red" ) # densidad de la curva f(x)
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(cauchy, "pcauchy")
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################