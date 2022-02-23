###########################Funcion a integrar###################################
G <- function(x){return(exp(-x^2))}
###########################Metodo Montecarlo####################################
mc <- function(G,a,b,M){
  s=0
  for (i in 1:M) {
    s <- s + G(a+(b-a)*runif(1,0,1))
  }
  return(((b-a)/M)*s)
}
################################################################################
mc(G,1,3,1000)
#G = funcion a integrar
#a = limite inferior
#b = limite superior
#M = Cantidad a simular
################################################################################
