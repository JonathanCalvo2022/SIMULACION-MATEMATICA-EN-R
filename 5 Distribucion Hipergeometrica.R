########################DISTRIBUCION HIPERGEOMETRICA############################
set.seed(1234)
adisc <- function(n, val, pr){
  if(length(val) == length(pr)){
    replicate(n, {
      u <- runif(1)
      Fx <- c(0, cumsum(pr))
      i <- min(which(u <= Fx) - 1)
      val[i]
    })
  } else{
    error("val y pr deben tener la misma longitud")
  }
}
################################################################################
a <- 0:3
fx <- c(10, 40, 30, 4) / 84
vrandom <- adisc(n = 200, val = a, pr = fx);vrandom
################################################################################
#Grafica 
barplot(fx,names.arg=0:3,space= 0,col="skyblue", main="Distribucion teorica")
################################################################################
#para esta prueba nesesitamos 
#x<- la frecuencia absolutas
#p <- probabilidades 
#nesesito las frecuencias
fabs <- table(vrandom)
################################################################################
chisq.test(x = fabs, p = fx)
#DECISION ESTADISTICA
#NO SE RECHAZA LA HIPOTESIS NULA
mean(vrandom)



