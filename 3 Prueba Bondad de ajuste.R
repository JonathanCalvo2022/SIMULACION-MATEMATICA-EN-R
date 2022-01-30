############################PRUEBA DE BONDAD DE AJUSTE##########################
set.seed(567)#Semilla
muestra <- runif(20)#100 observaciones
muestra
m<- length(muestra)
alpha <- 0.05 #Nivel de significancia
################################################################################
#1) Chi-Cuadrado--Distribuciones continuas y discretas
################################################################################
layout(matrix(c(1:2), nrow=2, byrow=FALSE))#para meter 2 graficas en 1
################################################################################
hist(muestra, col = rainbow(11))
hist(qnorm(muestra), freq= F)
curve(dnorm(x), add = T)
plot(muestra[1:(m-1)],muestra[2:m])
plot(muestra)
abline(h= 0:10/10, col = rainbow(11), lwd = 4)
################################################################################
layout(matrix(c(1:1), nrow=2, byrow=FALSE))#para meter 1 graficas en 1
################################################################################
a <- hist(muestra, breaks = 0:10/10,col = rainbow(11))
oj <- a$counts#Frecuencia Observada
Ej <- rep(m/10, 10)#Frecuencia Esperada
Estadistico.prueba <- sum((oj-Ej)^2/Ej)
criterio <- (Estadistico.prueba > qchisq(1-alpha, 9))
if(criterio == TRUE){
  "no son uniformes"
}else{
  "hay evidencia de uniformidad"
}
pvalue <- (1-pchisq(Estadistico.prueba,9))
pvalue # Rechazar la uniformidad si pvalue < alpha
################################################################################
#2) Kolmogorov-Smirlov--Distribuciones continuas
################################################################################
plot(ecdf(muestra),lwd=3,xlim=c(0,1),col='blue')
sort(muestra)
abline(h=0,v=0)
curve(punif(x,0,1),add = T,col=2,lwd=3)
ks.test(muestra,"punif") ##D es el estadistico de prueba

#calculo a mano
aux <- sort(muestra) #estadisticos de orden
points(aux,rep(0,m),pch=16,col='magenta',cex=1.2)
points(aux,0:(m-1)/m,cex=1.2)
alturas_linea_roja <- punif(aux)
alturas_linea_negra <- 1:m/m
distancias_izq <- abs(alturas_linea_negra-alturas_linea_roja)
distancias_der <- abs(0:(m-1)/m-alturas_linea_roja)
Estadistico.prueba <- max(distancias_izq,distancias_der)
Estadistico.prueba
aux2 <- which(c(distancias_izq,distancias_der) == Estadistico.prueba)
if(aux2>m){
  points(aux[aux2-m],(aux2-m-1)/m,pch=16,col='green',cex=2)
}else{
  points(aux[aux2],aux2/m,pch=16,col='green',cex=2)
}
#El punto verde nos muestra el valor mas alejado de la distribucion normal