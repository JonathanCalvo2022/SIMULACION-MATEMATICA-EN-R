## 2. Periodo

#El periodo nos permite identificar desde que posici�n de n�meros se vuelve a
#repartir nuestro muestra de valores aleatorios,para lo cual usaremos el m�todo
#congruencia para valores seudo-aleatorios.

m.cong <-function(n, seed, a, c, m){
  for (i in 1:n) {
    seed[i+1] <- (seed[i]*a+c)%%m
  }
  #seed
  (seed/m)
}

#n = 40, lo cual es la cantidad de numeros imprimidos en pantalla
#seed = 13, es nuestra semilla inicial
#c = 0, es el incremento
#m = modulo 

muestra <-m.cong(n = 40, seed = 13  ,a = 3, c = 0, m = 31)
muestra

#Seg�n se puede ver en los resultados imprimidos, se repite en la posicion 31,
#pero ahora usaremos un algoritmo en r para identificaci�n autom�ticamente.

#primero creamos el algoritmo donde se repetir� en bucle hasta identificar que 
#valor se parece al primer valor y luego al segundo y as� sucesivamente 

periodo <- function(x){
  dif<- 1
  i <- 1
  while (dif[i] != 0) {
    dif[i+1]<- x[i+1] - x[1]
    i <- i + 1
  }
  length(dif)
}

#Ahora aplicamos en la muestra

periodo(muestra)

#Como se puede ver nos dio similar como en la visualizaci�n inicial, d�ndonos
#el periodo 