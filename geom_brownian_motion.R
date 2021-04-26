Geom_Brownian <- function(M, N, d, t, mu, X0, Sigma){
  X <- Zmat <- deltas <- CV <- array(NA, dim = c(M, N + 1, d) ) 
  

  
  ## Set initial values = X0 
  X[,1,] <- X0 
  
  B <- t(chol(Sigma)) 
  dt <- t/ N 
  muMat <- matrix(rep(mu -0.5 * diag(Sigma) * diag(Sigma), M), 
                  ncol = d, byrow = T) 
  
  ## Run the simulation 
  for(i in 1:N){
    Z <- matrix(rnorm(d * M), ncol = M) 

    X[,i + 1,] <- X[,i,] * exp(muMat * dt + sqrt(dt) * t(B %*% Z)) 
    Zmat[,i + 1,] <- Z
    
    ## Calculate deltas 
    ttm <- t - dt * (i - 1) 
    deltas[,i,] <- Delta(X[,i,], K, 0.05, ttm, diag(Sigma))
    
    
  }
  deltas[,N+1,] <- delta(X[,N+1,],K,0.05,0,diag(Sigma))
  
  ## Generate Matrix of positions 
  CF[,1,] <- -deltas[,1,] * X0 
  for(i in 1:(N -1)){
    CF[,i + 1] <- -1 * (deltas[,i + 1,] - deltas[,i,]) * X[,i + 1,]
  }
  
  
  return(list('X' = X, 'Z' = Z, 'Delta' = deltas)) 
  
}


## Delta is change in stock vs change in price of underlying asset