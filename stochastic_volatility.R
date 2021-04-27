Stochastic.2d <- function(M, N, t,
                           mu, alpha, b, 
                           volvol, S0, V0) {
  dt <- t/N
  S <- V <- matrix(NA,ncol=N+1, nrow=M)
  S[,1] <- S0
  V[,1] <- V0
  
  for(i in 1:N){
    S[,i+1] <- S[,i] + 
      mu*S[,i]*dt + 
      sqrt(V[,i]*S[,i]) * sqrt(dt)*matrix(rnorm(M),ncol=1)
    
    V[,i+1] <- V[,i] + 
      alpha*(b-V[,i])*dt + 
      volvol*sqrt(V[,i])*sqrt(dt)*matrix(rnorm(M),ncol=1)
  }
  return(list("S"=S,"V"=V))
}



Stochastic <- function(M, N, t, mu, alpha, b, volvo1, S0, V0){
  ## change in time 
  dt <- t / N 
  sqdt <- sqrt(dt) 
  S <- V <- deltas <- array(NA, dim = c(M, N + 1, d)) 
  
  ## Set initial values 
  S[,1,] <- S0 
  V[,1,] <- V0 
  
  ## Simulate paths 
  for(i in 1:N){
    Z1 <- Z2 <- matrix(rnorm(d * M), ncol = M) 
    
    S[,i+1,] <- S[,i,] + mu*S[,i,]*dt +
      sqrt(V[,i,])*S[,i,]*sqdt*Z1
    
    V[,i+1,] <- V[,i,] + alpha*(b - V[,i,])*dt +
      volvol*sqrt(V[,i,])*sqdt*Z2
    
    ## Calculate deltas 
    ttm <- t - dt * (i - 1) 
    deltas[,i,] <- Delta(S[,i,], K, 0.05, ttm, volvo1) 
  }
  deltas[,N + 1,] <- Delta(X[,N+1,], K, 0.05, 0, volvo1) 
  
  return(list('S' = S, 'Z' = Z, 'Deltas' = deltas))
}





