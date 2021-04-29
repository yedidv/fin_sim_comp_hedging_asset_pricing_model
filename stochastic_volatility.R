Stochastic.deltas<- function(M, N, d, t, mu, a, b, volvol, S0, V0){
  ## change in time 
  dt <- t / N 
  sqdt <- sqrt(dt) 
  S <- V <- Sh <- array(NA, dim = c(M, N + 1, d)) 
  
  ## Set initial values 
  S[,1] <- Sh[,1] <- S0 
  V[,1] <- V0 
  
  h <- 0.1
  
  ## Simulate paths 
  for(i in 1:N){
    Z1 <- matrix(rnorm(M), ncol = 1) 
    Z2 <- matrix(rnorm(M), ncol = 1) 
    
    S[,i+1] <- S[,i] + mu*S[,i]*dt + sqrt(V[,i])*S[,i]*sqdt*Z1
    
    V[,i+1] <- V[,i] + a*(b - V[,i])*dt +
      V[i]*sqrt(V[,i])*sqdt*Z2
    
    Zmat <- Z1[i]
    Sh[,i+1] <- (S[,i] + 0.01) * exp(mu * dt + sqrt(dt) * t(V0 * Z1)) 
    
  }
  
  deltas <- call_put <- array(NA, dim = c(M)) 
  for(i in 1:M){
    ## Find the deltas for put and call options 
    ## We take the max delta for eacH time step for each option 
    ## create dataframe containing whether or not it's reccomended 
    ## to put or call at that period in time. 
    call_delta <- Delta(eput, S[i,], Sh[i,]) 
    put_delta <- Delta(ecall, S[i,], Sh[i,]) 
    call_put[i] <- ifelse(call_delta > put_delta, 'call', 'put') 
    deltas[i] <- pmax(call_delta, put_delta)/300
    
  }
  
  return(list('S' = S, 'V' = V, 'Deltas' = deltas))
}





