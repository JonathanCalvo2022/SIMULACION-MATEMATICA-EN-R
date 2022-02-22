################################################################################
#Caso Exponencial con parametro alpha=13
#FF <- function(x){pexp(x,13)}
#curve(FF(x),0,3,lwd=4,col='blue')
curve(pexp(x,13),0,1,lwd=4,col='blue')# exponencial no inversa
abline(h=0,v=0,lwd=4)
abline(h=1,lty=2,col=2,lwd=4)
#inversa
FFinv <- function(x){ 
  -log(1-x)/13 
}
curve(FFinv(x),0,0.9999,lwd=4,col='magenta',add=T)# inversa
curve(1*(x),add=T)#linea cental
################################################################################
#Primer caso metodo inversa exponencial
FFinv <- function(x){ 
  -log(1-x)/3 
}
u <- runif(10000)
Z <- FFinv(u)
hist(Z,freq = F,col=rainbow(8))
curve(dexp(x,3),add = T, col = 4, lwd=4)
################################################################################
#segundo caso metodo de inversa exponencial
v.a.exp <- function(n, lambda) {
  L <- -1/lambda
  u <- runif(n)
  x <- L * log(u)
}
z <- v.a.exp(10000, 3)
hist(z, prob = TRUE, breaks = 30)
y <- seq(0, 100, 0.1)
lines(y, lambda * exp(-lambda * y)) # density curve f(x)
################################################################################
#Media y varianza obtenido de la muestra
mean(Z)
var(Z)
#Media y varianza ideal teorica
media <- 1/13 ;media
varianza <- 1/13^2 ;varianza

