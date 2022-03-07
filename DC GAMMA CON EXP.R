set.seed(321)
################################################################################
v.a.exp <- function(n, lambda) {
  L <- -1/lambda
  u <- runif(n)
  x <- L * log(u)
}
###########################DISTRIBUCION GAMMA###################################
# GAMA(3,7)---------------------------------------------------------------------
v.a.gamma <- function(n,f,e){
  m <- v.a.exp(n,1/e)
  for (i in 1:(f-1)) {
    m <- m + v.a.exp(n,1/e)
    }
  m
}
################################PROMEDIO########################################
gamma<- v.a.gamma(n = 10^6, f=3,e=7 )
mean(gamma)   # MUESTRA REAL----------------------------------------------------
muestra <-rgamma(n = 10^6, 3,1/7)
mean(muestra) # MUESTRA TEORICA-------------------------------------------------
################################################################################
hist(gamma, main = expression(f(x) == e^(-t)*t^(Z-1)),probability = T)
curve(dgamma(x, 3,1/7),col="red",add = T)
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(gamma, "pgamma",3,1/7)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
################################################################################