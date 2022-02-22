################################################################################
#6) DISTRIBUCION WEIBULL CON SU INVERSA
potencia.3 <- function(w){
  return(sign(w) * abs(w)**(1/5))# alfa
} 
inv.weibull <-function(n,alfa,lambda){
  u<- runif(n)
  L<- 1/lambda
  x<- L*potencia.3(-log(u))
  x <- x[x != Inf & x != -Inf]
  x <- x[x>= 0]
} 
weibull <- inv.weibull(n = 10^6, alfa = 5,lambda = 1)
#################################################################################
lambda<- 1 
alfa <- 5
hist(weibull, prob = TRUE, main = expression(f(x) == alfa * lambda^alfa * y^(alfa - 1) * exp(-(lambda * y)^alfa) ) ) 
y <- seq(min(weibull), max(weibull), 0.01)
lines(y , alfa * lambda^ alfa * y^(alfa - 1) * exp(-(lambda * y)^alfa),col="red")
# densidad de la curva f(x)
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(weibull, "pweibull", shape = 5, scale = 1)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################
