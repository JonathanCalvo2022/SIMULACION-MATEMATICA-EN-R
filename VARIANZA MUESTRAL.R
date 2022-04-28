# Funcion Varianza Muestral
varianza.m <- function(x){
  n <- length(x)
  x.med <- mean(x)
  a<- ((x-x.med)^2)/(n-1)
  sum(a)
}
# Datos Muestrales 
xj <- c(110,97,100,105,108,99,118,104,105,103)
varianza.m(x = xj)
