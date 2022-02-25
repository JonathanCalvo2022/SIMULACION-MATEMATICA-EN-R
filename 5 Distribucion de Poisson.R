set.seed(3421)
################################################################################
#############################DISTRIBUCION DE POISSON############################
################################################################################
#############################Distribucion poison real###########################
################################################################################
ncomp <- 0
rfmp <- function(lambda, nSim) {
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
system.time( rx <- rfmp(lambda= 2,nSim=10^5) )
################################################################################
lambda<-2#Teorico
nSim<-10^6
mean(rx) # Aproximacion de la media
var(rx) # Aproximacion de la varianza
################################################################################
ncomp/nSim # Numero medio de comparaciones
# Analisis de resultados
res <- table(rx)/nSim
plot(res, ylab = "frecuencia relativa", xlab = "valores")
fmp <- dpois(x = 0:max(rx),lambda = lambda)
points(0:max(rx), fmp, pch = 4)  # Comparacion teorica
res <- as.data.frame(res)
names(res) <- c("x", "psim")
res$pteor <- fmp
res
################################################################################
################Poisson teorico(resultado al que se debe llegar)################
################################################################################
pois.teorico <-function(n,lambda){
  s <-3*sqrt(lambda)
  t <-round(seq(max(0,lambda-s),lambda + s,1))
  prob <- ppois(t,lambda)
  x<-rep(0,n)
  for(i in 1:n){
    u <- runif(1)
    x[i] <- t[1]+ sum(prob <u)
  }
  x
}
poiss <- pois.teorico(n=10^6,lambda = 2)
mean(poiss)
################################################################################
plot.ecdf(pois.t)
