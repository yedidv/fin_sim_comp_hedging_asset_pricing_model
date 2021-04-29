

Brown_Motion <- function(M, N, t, X0, mu, sigma, h = 0.01){
  musim <- mu - 0.5 * sigma * sigma
  dt <- t / N 
  X <- Xh1 <- Xh2 <- deltas <- matrix(NA, ncol = (N + 1), nrow = M) 
  X[,1] <- X0 
  deltas[,1] <- 0 
  
  ## run simulation 
  for(i in 1:N){
    Z <- rnorm(M) 
    X[,i + 1] <- X[,i] * exp(musim * dt + sigma * sqrt(dt) * Z) 
    
    ## Finite differences 
    forward <-  (X[,i] + h)* exp(musim * dt + sigma * sqrt(dt) * Z) 
    backward <-  (X[,i] - h)* exp(musim * dt + sigma * sqrt(dt) * Z)
    
    deltas[,i + 1] <- Delta(ecall, backward, forward, h, t) 
    
  }
  return(
    list('Deltas' = deltas, 'X' = X) 
  )
}




