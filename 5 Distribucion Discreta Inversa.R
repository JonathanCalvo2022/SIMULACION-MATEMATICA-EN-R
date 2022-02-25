########################TRANSFORMADA INVERSA DISCRETA###########################
discrete.inv.transform.sample <- function( p.vec ) {
  U  <- runif(1)
  if(U <= p.vec[1]){
    return(1)
  }
  for(state in 2:length(p.vec)) {
    if(sum(p.vec[1:(state-1)]) < U && U <= sum(p.vec[1:state]) ) {
      return(state)
    }
  }
}
#################################EJEMPLO########################################
num.samples <- 1000
p.vec        <- c(0.1, 0.4, 0.2, 0.3)
names(p.vec) <- 1:4
samples     <- numeric(num.samples)
for(i in seq_len(num.samples) ) {
  samples[i] <- discrete.inv.transform.sample(p.vec)
}
barplot(p.vec, main='True Probability Mass Function')
################################################################################
barplot(table(samples), main='Empirical Probability Mass Function')
################################################################################
