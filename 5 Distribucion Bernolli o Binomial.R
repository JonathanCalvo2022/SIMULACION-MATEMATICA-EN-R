############################DISTRIBUCION BERNOLI################################
set.seed(3214)
bin<- function(n,p){
  q<-1-p
  u<-runif(n)
  z<- as.integer(u > q)
  z
}
binomial.real <- bin(n=1000, p = 0.4)
################################GRAFICO#########################################
n<-1000
##########################PROMEDIO TEORICO######################################
binomial.teorica<- rbinom(n, size = 1, prob = 0.4)
mean(binomial.teorica)
###########################PROMEDIO REAL########################################
mean(binomial.real)
################################################################################

