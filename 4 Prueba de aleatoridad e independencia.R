# 4 Prueba de aleatoriedad e independencia
# 4.1 Contraste de rachas
#     Corridas por abajo y arriba del promedio
# Ho: Las observaciones son independientes
# Ho: Las observaciones son aleatorias
# Ho: Existe Aleatoriedad(independencia entre los datos)
###############################################################################
#Caso automatico
#install.packages("randtests")
library(randtests)
runs.test(muestra)
###############################################################################
#Caso manual
set.seed(123)
muestra <- runif(500)
m <-length(muestra)
alpha <- 0.05
#####################################Grafica################################### 
plot(muestra,main = "Grafico de dispersión ")
media<-median(muestra);media#media o promedio de los datos
abline(h = media,col = "red")
###############################################################################
n1<-length(muestra[muestra<media])#Num.obs en [0,mediana)
n2<-length(muestra[muestra>media])#Num.obs en (mediana,1]
indicadores <- 1
for(i in 2:m){
  aux1<-1*(muestra[i-1]<media)
  aux2<-1*(muestra[i]<media)
  indicadores[i]<-1*(aux1 != aux2)
}
rachas<-sum(indicadores)
E.R <-(2*n1*n2)/(n1+n2)+1#Esperanza o mediana de las rachas
VAR.R <-(2*n1*n2*(2*n1*n2-n1-n2))/((n1+n2)^2*(n1+n2-1))#Varianza de rachas
z<-(rachas-E.R)/sqrt(VAR.R);z#Estadistico de prueba
p.valor<-2*(1-pnorm(z));p.valor# valor de p
#Condiciones
q<- qnorm(1-alpha/2)#q(1-alpha/2) = 1.959964
#Rechazar la Ho si |z|>=q(1-alpha/2) , p.valor<alpha
###############################################################################
if(abs(z)>=q && p.valor< alpha){
  "Rechazar la Hipotesis nula"
}else{
  "No se rechaza la Hipotesis nula"
}
###############################################################################

