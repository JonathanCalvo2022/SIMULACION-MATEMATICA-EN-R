set.seed(321)
#DISTRIBUCION EXPONENCIAL
inv.exp <- function(n, lambda){
  u <- runif(n)
  -log(1 - u) / lambda
}
################################################################################
#2) DISTRIBUCION NORMAL CON EXPONENCIAL
inv.normal <- function(n){
  k <- 0      # cuenta el n???mero de valores generados
  j <- 0      # iteraciones
  x <- numeric(n)
  while (k < n) {
    y <- inv.exp(1, 1)  # Envolvente Y
    u <- runif(1)
    j <- j + 1
    if (u <= exp((-1/2) * (y-1)^2)) { # se acepta el valor y
      s <- sample(x = c(-1,1),size = 1, prob = c(.5,.5))
      k <- k + 1
      x[k] <- s * y
    }
  }
  x
}
normal<- inv.normal(10^6)
###############################################################################
hist(normal, main = "DISTRIBUCION NORMAL", probability = T)
curve(dnorm(x), col = "red", add = T)#Distribucion teorica
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(normal, "pnorm")
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
