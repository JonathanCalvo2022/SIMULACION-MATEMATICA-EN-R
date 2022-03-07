# Distribucion logistica(Continua)
set.seed(3421)
n<-1000
u <- runif(n)
x<- 1-(5*log((1/u) - 1))
y<- rlogis(n, location = 1, scale = 5)
mean(x)
mean(y)

hist(x, nclass = 50)
hist(y, nclass = 50)

################################################################################
#Binomial discreta
#x <- Bin(15,0.4)
n<- 15
nSim <- 100
p <-rep(0,n)
t <- c(0:nSim)
prob <- pbinom(q = t, size = n, prob = 0.4)
x<- rep(0, nSim)
for(i in 1:nSim){
  u <- runif(1)
  x[i]<-t[1] + sum(prob<u)
}

inv.bin <- function(nSim,n,prob){
  p<-rep(0,n)
  t<-c(0:nSim)
  x<- rep(0,nSim)
  for (i in 1:nSim) {
    u <- runif(1)
    x[i]<-t[1]+sum(prob<u)
  }
  x
}
bin<-inv.bin(nSim = 10^6, n = 15, prob = 0.4)
mean(bin)

w<-rbinom(10^6, size = 15 , prob = 0.4)
mean(w)



barplot(prop.table(table(x)), main = "user defined code")
barplot(prop.table(table(rbinom(nSim, size = n, prob = 0.4))))
################################################################################
#############################Distribucion poison real###########################
################################################################################
set.seed(3421)
ncomp <- 0
bn <- function(nsim,x,p) {
  binom <- numeric(nsim)
  u <- runif(nsim)
  for(j in 1:nsim) {
    k <- 0
    Fx <- 0
    B <- T
    while (B==T) {
      p <- exp(-lambda)*(lambda^k)/(factorial(k))
      val <- (p^x)*((1-p)^(1-x))
      Fx <- Fx + val
      if (Fx < u[j]) {
        k <- k + 1
      } else {
        poisson[j] <- k
        B <- F
      }
    }
    ncomp <<- ncomp + k + 1
  }
  return(binom)
}


system.time( rx <- rfmp(lambda= 2,nsim=10^6) )
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
# combrobacion de r
ter<-rpois(n = 10^6, lambda = 2)
mean(ter)
################################################################################
# comparacion de medias
mean(rx) # Aproximacion de la media del profesor
mean(ter) # Aproximacion del programa R


######################################################################################
geometrica <- function(p){
  K = 1
  while(runif(1) > p) {
    K = K + 1
  }
  K
}
g <- replicate(geometrica(0.7), n =  10^4); g
#[1] 1.4281
################################################################################
gen <- function(p){
  
  geom<-numeric()
  u <- runif(n)
  for (i in 1:n){
    k<-1
    Fx<-0
    B<-TRUE
    while (B==TRUE) {
      Fx <- (1 - ((1-p)^(k+1)))
      if(Fx<u[i]){
        k<-k+1
      }else{
        geom[i]<-k
        B<-FALSE
      }
    }
  }
  return(geom)
}
genoma <-gen(n=10^4, 0.3)
set.seed(789)
mean(genoma)#mio
mean(g)#priofe

