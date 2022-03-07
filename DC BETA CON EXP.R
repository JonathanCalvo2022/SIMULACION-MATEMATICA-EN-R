set.seed(321)
##########################DISTRIBUCION EXPONENCIAL##############################
v.a.exp <- function(n, lambda) {
  L <- -1/lambda
  u <- runif(n)
  x <- L * log(u)
}
#############################DISTRIBUCION BETA##################################
##BETA(5,3) con lambda = 1
#ncomp <- 0
v.a.beta <- function(n, k1, k2, lambda){
  g1 <- rowSums(replicate(k1, v.a.exp(n,lambda)))
  g2 <- rowSums(replicate(k1+k2, v.a.exp(n,lambda)))
  #ncomp <<- ncomp + n + 1
  g1/(g2)
}

beta <- v.a.beta(n= 10^6,k1=5,k2 = 3,lambda = 1)
mean(beta)
##################################GRAFICA#######################################
#Histograma
hist(beta,  prob = TRUE, xlim = c(0, 1),col = "slategray2",border = "white",
      main = expression(f(x) == x^(p-1)*(1-x)^(q-1)), breaks = 1000,
     xlab = "Distribucion Beta", las = 1) 
# Función de densidad
lines(density(beta), # density plot
      lwd = 2, # thickness of line
      col = "darkblue")
################################################################################
mean(beta) # PROMEDIO DEL PROFESOR
################################################################################