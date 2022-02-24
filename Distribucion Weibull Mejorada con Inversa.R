#############################INVERSA WEIBULL####################################
set.seed(321)
weibull <- function(n,a,b){
  U <- runif(n)
  X <- (-(1/b^a)*log(U))^(1/a)
}
#Ahora probemos para ??=2, ??=3 generemos 1000 valores.
X <-weibull(n = 1000,a = 2,b = 3)
#Realizando un histograma y comparando los resultados tenemos lo siguiente
hist(X,freq = F)
curve(dweibull(x,shape = 2,scale = 1/3),add=T,col=2)
#Realizando la prueba de bondad de ajuste se tiene que Kolmogorov-Smirnov
ks.test(X,"pweibull",shape=2,scale=1/3)
################################################################################
