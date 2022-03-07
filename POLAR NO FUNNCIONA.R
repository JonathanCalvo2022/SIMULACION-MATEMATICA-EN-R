#################################DISTRIBUCION POLAR#########################
apolar<- function(n){
   u1<-runif(n)
   u2<-runif(n)
   v1<- (2*u1 -1)
   v2<- (2*u2 -1)
   w <- (v1^2)*(v2^2)
   
   if(w > 1){
     y <- sqrt((-2*w)/w)
   }else{
   }
   y
   x1<- v1*y
   x2<- v2*y
 }
apolar(n= 10^6)
 