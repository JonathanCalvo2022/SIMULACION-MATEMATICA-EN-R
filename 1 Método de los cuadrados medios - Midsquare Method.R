#Primer ejemplo
cant <- 8
cons <- function(a){
  #nuestra semilla lo transformamos a carácter
  nn <- as.character(a)
  #incrementamos el espacio en nuestro muestra
  esp <- strsplit(nn , "")
  #generamos una lista de nuestra semilla
  x1 <- unlist(esp)
  #Ahora contamos la longitud de los datos
  num <- length(x1)
  #Aplicamos un bucle re petitorio
  if(num < cant){
    num1<- c(rep(0,( cant - num)), x1)
    #En caso de cumplirse imprimir
    num1 
  }
  #En caso de no cumplirse
  x1
}
#Aplicamos el Método de cuadrados medios en r

seed <- 5497
n <- 4
#Método de los cuadrados medios
#Creamos la función para los mínimos cuadrados
msq <- function(n, seed){
  for(i in 1:n){
    #Aplicamos un denominador,para dividir con la semilla
    den <- 10000
    #para que exista centro aplicamos valores de los extremos
    #en este caso desde el 3 a 6
    cons.n <-cons(seed[i]^2)[3:6]
    seed[i + 1] <- as.numeric(paste(cons.n, sep = " ",collapse = ""))
  }
  (seed / den)[-1]
}
msq(n ,seed)

#Segundo ejemplo(Caso Simplificado)
#Función seudo-aleatoria (0,1)

cons <- function(a){
  x1 <- unlist(strsplit(as.character(a) , ""))
  num <- length(unlist(strsplit(x1 , "")))
  if(num < 8){
    c(rep(0,( 8 - num)), x1)
  }
  x1
}
#Metodo de los cuadrados medios

#Creamos la funcion para los minimos cuadrados
msq <- function(n, seed){
  for(i in 1:n){
    seed[i + 1] <- as.numeric(paste( cons(seed[i]^2)[3:6],collapse = ""))
  }
  (seed / 10000)[-1]
}
#Agregamos valores: Cantidad = 4, Semilla = 5497
msq(n = 4,seed = 5497 )