################################################################################
#6) DISTRIBUCION WEIBULL CON SU INVERSA
################################################################################
#PRIMERA FORMA
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
################################################################################
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
#EJEMPLO PARACTICO: 

#Usando el método de la inversa, cree una R función que permita crear números
#provenientes de esta distribución.
############f(x) = (k/lambda)((x/lambda)^(k-1))(e^(-(x/lambda)^k))##############
aweibull <- function(n, k, lambda){
  u <- runif(n)
  lambda*(-log(1 - u))^(1/k)
}
# Cree una muestra de 10000 (diez mil) números aleatorios, usando la función de la 
# Pregunta 1. Mediante un gráfico pruebe que la distribución de la muestra se
# ajusta a la distribución teórica de weibull. Use parámetros k = 2, lambda = 1/2.
set.seed(1234)
muestra <- aweibull(n = 10000, k = 2, lambda = 1/2)
hist(muestra, probability = T, main = '', xlab = '', ylab = '')
curve(dweibull(x, shape = 2, scale = 1/2), col = 'red', add = T)
# Mediante una prueba de bondad de ajuste y usando los datos creados en la 
# Pregunta 2, contraste la hipótesis:
# H0 : Los datos provienen de una Weib(k = 2, lambda = 1/2) Vs 
# H1 : Los datos no provienen de una Weib(k=2, lambda = 1/2)
ks.test(muestra, 'pweibull', shape = 2, scale = 1/2)
# Como pval > alpha, no rechazamos H0 y concluimos que no existe evidencia
# suficiente para decir que los datos
# provienen de una distribución distinta a la de Weibull.
################################################################################

