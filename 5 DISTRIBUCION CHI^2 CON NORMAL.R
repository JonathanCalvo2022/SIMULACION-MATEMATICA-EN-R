set.seed(321)
#1) DISTRIBUCION EXPONENCIAL
inv.exp <- function(n, lambda){
  u <- runif(n)
  -log(1 - u) / lambda
}
################################################################################
#2) DISTRIBUCION NORMAL 
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
################################################################################
#3) DISTRIBUCION CHI^2 CON NORMAL
inv.chi <- function(n,gl){
  m <- gl*n
  Normal <- inv.normal(m)
  Normal2 <- Normal^2
  dim(Normal2) <- c(n,gl);
  chi <- rowSums(Normal2,na.rm = T)
  chi
}
chi.cu <- inv.chi(n = 10^6 ,gl = 4)
hist(chi.cu , main = "DISTRIBUCION CHI^2", probability = T)
curve(dchisq(x,df=4), col = "#458B74", add = T)#Dsitribucion teorica
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(chi.cu, "pchisq", df = 4)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)
