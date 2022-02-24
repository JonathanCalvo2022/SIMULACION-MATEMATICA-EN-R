# USO DE REPLICATE() EN R
################################################################################
n <- numeric(5)
m<- replicate(5,{
  for (i in 1:length(n)) {
    n[i]<- i
  }
  n
})
m
#############################EJEMPLO############################################
# La probabilidad de que el proximo nacimiento en un hospital de un ninio es de
# 0.52, en los proximos mil nacimientos que produzcan en dicho hospital, vamos
# a calcular la probabilidad de que haya mas de 540 ninios
# 
# Por un lado podriamos usar la distribucion binomial, con n = 1000, p = 0.52 
# para calcular ese valor, pero lo que vamos a hacer aqui es simular esos 1000
# nacimientos en un hospital y ver cuantos ninios btendremos
rbinom(n = 1, size = 1000,prob = 0.52)
# Si queremos saber lo que ocurre en un hospital concreto, y si queremos saber
# en 300 hospitales?, usaremos replicate
num_hospitales <- 300
n <-num_hospitales

hospitales <- replicate(n,{
  rbinom(n =1, size = 1000, prob = 0.52)
})
hospitales
# En que fraccion de estos hospitales se supera la cifra de 540 ninio?
sum(hospitales>540)/n
# La probabilidad calculada con la binomial es:
1- pbinom(540, size = 1000, prob = 0.52)
######################FIN DEL CODIGO############################################






