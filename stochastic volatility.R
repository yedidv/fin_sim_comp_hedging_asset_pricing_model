Stochastic.vol <- function(M, N, t, mu, alpha, b, volvol, S0, V0) {
  dt <- t/N
  S <- V <- matrix(NA,ncol=N+1, nrow=M)
  S[,1] <- S0
  V[,1] <- V0
  
  for(i in 1:N){
    S[,i+1] <- S[,i] + mu*S[,i]*dt + sqrt(V[,i]*S[,i]) * sqrt(dt)*matrix(rnorm(M),ncol=1)
    V[,i+1] <- V[,i] + alpha*(b-V[,i])*dt + volvol*sqrt(V[,i])*sqrt(dt)*matrix(rnorm(M),ncol=1)
  }
  return(list("S"=S,"V"=V))
}

#
M <- 10000
N <- 252
t <- 1

# mu
# alpha
# b
# volvol
# V0
# S0