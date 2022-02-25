############################Distribucion Box-Muller#############################
set.seed(321)
################################################################################
# import the library to test the normality of the distribution
library(nortest)
################################################################################
inv.b.m <- function(size){ 
  u = runif(size)
  v = runif(size)
  
  x=rep(0,size)
  y=rep(0,size)
  
  for (i in 1:size){
    x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
    y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
  }
  c(x,y)
}
bs <- inv.b.m(size = 100000)
####################################GRAFICA#####################################
hist(bs, main = "Distribucion Box-Muller", probability = TRUE)
curve(dnorm(x),col = "red", add = T)
#####################################PRUEBA#####################################
#a test for normality
lillie.test(bs)
#p.value > 0.05 No se rechaza la H0:(Sigue una distribucion normal)
#####################################FIN########################################
