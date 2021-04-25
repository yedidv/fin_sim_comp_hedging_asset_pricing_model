Geom_Brownian <- function(M, N, d, t, mu, X0, Sigma){
  X <- Zmat <- array(NA, dim = c(M, N + 1, d) ) 
  
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
    
  }
  
  return(list('X' = X, 'Z' = Z)) 
  
}

