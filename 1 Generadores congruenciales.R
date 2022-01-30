#1) Generadores congruenciales

#1.1) Generador Congruencial lineal Mixto
# cuando tiene c 
  
#1.2) Generador Congruencial multiplicativo o 
# generador de numeros pseudoaleatorios de Lehmer
# cuando c = cero
  
#xi = numero anterior semilla
#a = multiplicador
#c = incremento
#m = modulo
  
#Generador para el lineal y el multiplicativo
m.cong <- function(x0,a,c,m,M){
  aux<-numeric(m)
  aux[1]<-x0
  for(i in 2:m){
    aux[i]<-(aux[i-1]*a+c)%%M
  }
  #aux #para valores de Lehmer
  (aux/M)[-1] #Variables aleatorias
}
#1.1) Generador congruencial lineal
#Semilla 1 , valores 17
m.cong(x0 = 1, a= 6, c=9, m =17 ,M= 100)

#1.2) Generador congruencial  multiplicativo
#Semilla 13 , Valores Aleatorios 31
m.cong(x0 = 13, a= 3, c=0, m =31 ,M= 100)

#1.3) Generador congruencial combinado
cong.mix <- function(n,a,m,seed){
  k <- length(a)
  ri<- NULL
  for(i in 1:n){
    seed <- a*seed%%m
    xi <- sum((-1)^(1:k-1)*seed)%%m[1]
    ri[i] <- ifelse(xi==0,(m[1]-1)/m[1],xi/m[1])
  }
  ri
}
cong.mix(n = 10,a = c(2,11,5),m = c(16,32,128),seed = c(12,7,120))


