##############DISTRIBUCION DE UNIFORMIDAD CONTINUO##############################
# n = numero de simulaciones;a = se refiere al minimos;b = se refiere al maximos
set.seed(321)
v.a.unif <- function( a, b){
  u <- runif(1)
  if(a < b){
    a + (u * (b - a))
  } else{
    error("a debe ser el minimo")
  }
}
uniforme <- replicate(n=10000, {
  v.a.unif(a = 0, b = 1)
})
mean(uniforme)# MEDIA REAL
z<-runif(n=10000,min = 0, max = 1)
mean(z)# MEDIA TEORICA
##############################GRAFICA###########################################
hist(uniforme, probability = T, main = expression(f(x) == frac(1,b-a)),
     col = "skyblue")
curve(dunif(x, 0, 1), 0, 1, col = "red", add = T)
# prueba de bondad de ajuste (Kolmogorov-Smirlov)-------------------------------
ks.test(uniforme, "punif",x=10000,min = 0, max = 1)
#p.value > 0.05 No Rechazo la Ho: (hay evidencia de uniformidad y son independientes)
################################################################################






