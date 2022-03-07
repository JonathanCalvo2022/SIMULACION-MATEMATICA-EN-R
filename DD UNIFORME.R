set.seed(3421)
######################DISTRIBUCION UNIFORME DISCRETA############################
ncomp <- 0
rfmp <- function(k, nsim) {
  unif <- numeric(nsim) #[-1]
  u <- runif(nsim)
  for(j in 1:nsim) {
    n <- 1
    Fx <- 0
    B <- T
    while (B==T) {
      p <- 1/k
      Fx <- Fx + p
      if (Fx < u[j]) {
        n <- n + 1
      } else {
        unif[j] <- n
        B <- F
      }
    }
    ncomp <<- ncomp + n + 1
  }
  return(unif)
}
rx <- rfmp(k= 6 ,nsim =10^5);rx
################################################################################
mean(rx) # MEDIA REAL
e<-runif(n=10^5, min = 1,max = 6)
mean(e)  # MEDIA TEORICA
###############################GRAFICA DE LA FUNCION############################
hist(rx, main = expression(f(x) == frac(1, k)))
#######################DISTRIBUCION DE POISSON##################################
# GRAFICA ---------------------------------------------------------------------
k <- 6
nsim<-10^5
ncomp/nsim # Numero medio de comparaciones
# Analisis de resultados
res <- (table(rx)/nsim)
plot(res, ylab = "frecuencia relativa", xlab = "Uniforme Discreta",
      main = expression(f(x) == frac(1, k)))
################################################################################
#PUNTOS ------------------------------------------------------------------------
fmp <- dunif(x = 1:max(rx) ,min = 0, max = 6)
fmp
points(x = 1 : max(rx), fmp, pch = 4, col= "red")  # Comparacion teorica
#TABLA DE VALORES A COMPARAR ---------------------------------------------------
res2 <- as.data.frame(res)
names(res2) <- c("x", "psim")
res2$pteor <- fmp
res2
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
  "Rechazo la H0: (no existe evidencia de uniformidad)"
}else{
  "No Rechazo la Ho: (hay evidencia de uniformidad)"
}
################################################################################

