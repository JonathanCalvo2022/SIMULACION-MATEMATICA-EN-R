#Generador lineal congruencial simple
#Generador de numeros aleatorios 
m <- 100
x0 <-11
M<-37
a <- 5
c<- 0
##################################
aux<-numeric(m)
aux[1]<- x0
for(i in 2:m){
  aux[i]<- (aux[i-1]*a+c)%%M
  
}
aux ##succesion de Lehmer
u<- aux/M
plot(u,pch=16, cex=2) 

#########Automatico##############

cong <- function(m,x0,a,c,M){
  aux<-numeric(m)
  aux[1]<-x0
  for(i in 2:m){
    aux[i]<-(aux[i-1]*a+c)%%M
  }
  #aux #para Lehmer
  aux/M #para variables aleatorias
}

muestra <- cong(10000,13,3,0,31)
muestra

plot(muestra)
hist(muestra)
mean(muestra)
var(muestra)
##################################
#Periodo 
period <- function(x){
  dif <- 1
  i <- 1
  while (dif[i] != 0) {
    dif[i+1]<- x[i+1] - x[1]
    i <- i +1
  }
  length(dif)-1
}

period(muestra)
################################

muestra

#La obtencion del numero de rachas
rachas <-vector()
for(i in 1:length(muestra)-1){
  rachas[i]<- sign(muestra[i+1]-muestra[i])
  n1 <- length(rachas[rachas[]>0])
  n2 <- length(rachas[rachas[]<0])
  m <- n1 + n2
  R = 1
  for(i in 1: (length(muestra)-2)){
    if(sign(rachas[i]) != sign(rachas[i+1]))
      R = R+1
  }
  
}



