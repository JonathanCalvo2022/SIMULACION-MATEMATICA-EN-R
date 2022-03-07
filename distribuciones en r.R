
###########################DISTRIBUCION LOGISTICA###############################
# x~log(alpha=1, beta=5)
n<-10000
u<-runif(n)
x<-1-5*log(1/u -1)
y<-rlogis(n, location = 1, scale = 5)
hist(x, nclass = 50)
hist(y, nclass = 50)
###########################DISTRIBUCION BOX-MULLER##############################
# x~N(u = 15 , sigma = 5)
n<-5
mu<-c(1,12,2)
sigma<-matrix(c(1,2,0,2,5,0,5,0,0.5,3),3,3)
A<-chol(sigma)
y<-matrix(rnorm(15),3,5)
x<-A %*% y + mu
x
library(mnormt)
t(rnorm(5,mu,sigma))

############################DISTRIBUCION BINOMIAL###############################
# x~Bin(15, 0.4)
n<-15
nSim<-1000
p<-rep(0,n)
t<-c(0:n)
prob <- pbinom(q = t, size = n, prob = 0.4)
x<- rep(0,nSim)
for(i in 1:nSim){  
  u <- runif(1)
  x[i]<- t[1] + sum(prob <u)
}
barplot(prop.table(table(x)), main = "User defined code")
barplot(prop.table(table(rbinom(nSim, size = n,prob = 0.4))),main = "by rbinom()")

###########################DISTRIBUCION GEOMETRICA##############################
# X~G(p)
v.a.g<- function(n,lamda=p){
  normal<-runif(n)
  digeo<-c()
  if(lamda > 0){
    digeo<- (-log(1 - normal)) / lamda
  }
}
g <- v.a.g(n=10^4, lamda= 0.7)
mean(g)
hist(g, main = "Distribucion Geometrica",probability = T)
curve(dgeom(x,prob = 0.7),col="red",add = T) 
#Prueba Analitica(kolmogorov-Smirlov)
ks.test(g, "pgeom",prob=0.7)
#p.value>0.05 No rechazar H0:(sigue la misma distribucion normal)

#######################DISTRIBUCION BINOMIAL NEGATIVA###########################
# X~Neg(n,p)
nSim<-10000
n<-6
p<-0.3
y<-rgamma(nSim,n,rate = p/(1-p))
x<-rpois(nSim,y)
hist(x,main = "",freq = F,col = "red", breaks = 40)
lines(1:50, dnbim(1:50,n,p), lwd=2, col ="green")
#############################DISTRIBUCION DE POISSON############################
# X~Pois(p)
###############################DISTRIBUCION BETA################################
# X~Betta(6,3)
xx <-runif(1e+05,0,1)
accept <-c()
for (i in 1:length(xx)) {
  u<-runif(1,0,1)
  if(dunif(xx[i],0,1)*3*u<=dbeta(xx[i],6,3)){
    accept[i]<- "SI"
  }else if(dunif(xx[i],0,1)*3*u > dbeta(xx[i],6,3)){
    accept[i]<-"NO"
  }
}
yy <- data.frame(xx, Accept = factor(Accept,levels = c("YES","NO")))
x<-seq(0,1,length.out =10000)
sq <- seq(0,1,0.01)
hist(yy[yy$Acept=="YES",1],breaks = sq, freq = F, main = "Histogram of xx",xlab ="xx")
lines(x, dbeta(x,6,3),lwd=2,col = "blue")
library(ggplot2)
print(qplot(xx,data = yy, geom = "Density",color = Accept))
print(qplot(xx,data = yy, geom= "histogram",fill = Accept, binwidth = 0.01))
##############################DISTRIBUCION PARETO###############################
alpha<- 19.5
beta<- 15.7
##definir el objetivo de densidad
targetPareto <- function(x){
  beta^alpha*alpha*(beta+x)^(-alpha-1)
}
M<-10/7 ##scale value
## Pareto kernel times M can be covered by exp(lambda =1)
x <- 1:600/100
plot(x, dexp(x),type = "l", ylab = "f(x) and g(x)",col = 1)
lines(x, M*targetPareto(x), col = 2, lty = 2)
legend("topriht", legend = c("g(x)","Mf(x)"),lty = 1:2, col = 1:2)
temp = rexp(10000,1)
temp = temp[temp<6]
u<- runif(length(temp), 0 , dexp(temp))
y<-temp[u<=0.7*targetPareto(temp)]
hist(y, prob = T, nclass = 20, xlab = "Value", main = "Rejection Sampling Method")
lines(x, targetPareto(x), col = "blue")
legend("topright", "Pareto kernel", lty = 1, col = "blue")







































