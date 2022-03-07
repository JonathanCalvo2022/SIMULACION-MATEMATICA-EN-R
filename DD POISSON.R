set.seed(3421)
#######################DISTRIBUCION DE POISSON##################################
ncomp <- 0
rfmp <- function(lambda, nsim) {
  poisson <- numeric(nsim)
  U <- runif(nsim)
  for(j in 1:nsim) {
    k <- 0
    Fx <- 0
    B <- T
    while (B==T) {
      p <- exp(-lambda)*(lambda^k)/(factorial(k))
      Fx <- Fx + p
      if (Fx < U[j]) {
        k <- k + 1
      } else {
        poisson[j] <- k
        B <- F
      }
    }
    ncomp <<- ncomp + k + 1
  }
  return(poisson)
}
rx <- rfmp(lambda= 2,nsim =10^5)

################################################################################
mean(rx) # Aproximacion de la media real
m<-rpois(n = 10^5,lambda = 2)
mean(m)  # Aproximacion de la media teorica
################################################################################
## GRAFICA ---------------------------------------------------------------------
lambda<-2
nsim<-10^5
ncomp/nsim # Numero medio de comparaciones
# Analisis de resultados
res <- (table(rx)/nsim)
plot(res, ylab = "frecuencia relativa", xlab = "Poisson",
     main = expression(f(x) == frac((e^-k)*k^x, fact(x))))
################################################################################
#PUNTOS ------------------------------------------------------------------------
fmp <- dpois(x = 0:max(rx),lambda = lambda)
points(0:max(rx), fmp, pch = 4, col= "red")  # Comparacion teorica
#TABLA DE VALORES A COMPARAR ---------------------------------------------------
res1 <- as.data.frame(res)
names(res1) <- c("x", "psim")
res1$pteor <- fmp
res1
#####################PRUEBA DE BONDAD DE AJUSTE#################################
alpha <- 0.05
prop <- sum(rx)/length(rx)
p1 <-prop/sum(rx)
pr <- rep(p1, length(rx))
chi <- chisq.test(rx)
###############################################################################
# prueba chi-cuadrado-----------------------------------------------------------
Estadistico.prueba <- chi$p.value 
criterio <- (Estadistico.prueba > qchisq(1-alpha, 9))
if(criterio == TRUE){
  "Rechazo la H0: (no son uniformidad, ni independientes)"
}else{
  "No Rechazo la Ho: (hay evidencia de uniformidad y son independientes)"
}
################################################################################


