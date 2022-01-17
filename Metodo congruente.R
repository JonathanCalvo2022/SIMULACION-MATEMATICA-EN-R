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