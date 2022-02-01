#Caso Exponencial con par???metro alpha=13
#FF <- function(x){pexp(x,13)}
#curve(FF(x),0,3,lwd=4,col='blue')
curve(pexp(x,13),0,1,lwd=4,col='blue')
abline(h=0,v=0,lwd=4)
abline(h=1,lty=2,col=2,lwd=4)
#inversa
FFinv <- function(x){ -log(1-x)/13 }
curve(FFinv(x),0,0.9999,lwd=4,col='magenta',add=T)
curve(1*(x),add=T)
########################################
## Generamos una muestra tama???o 10000 ##
########################################
u <- runif(10000)
Z <- FFinv(u)
hist(Z,freq = F,col=rainbow(8))
curve(dexp(x,13),add = T, col = 4, lwd=4)
plot(ecdf(Z),col = 6,lwd=5)
curve(pexp(x,13),add=T,col=1,lwd=2)
mean(Z)
var(Z)
media <- 1/13
media
varianza <- 1/13^2
varianza
var(Z)
##### Usando R
Zcontrampa <- rexp(10000,13)
######################################
qqplot(Z,Zcontrampa)
curve(1*x,add=T,col=2)
hist(Z)
hist(Zcontrampa,add=T)
