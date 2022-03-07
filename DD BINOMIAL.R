############################DISTRIBUCION BINOMIAL###############################
# x~Bin(15, 0.4)
set.seed(1234)
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
################################################################################
