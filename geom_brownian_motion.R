Geom_Brownian <- function(M, N, d, t, mu, X0, Sigma){
  X <- array(NA, dim = c(M, N + 1, d) )
  Xh <- Zmat <- CV <- call_payoff <- put_payoff<-  X
  
  
  
  ## Set initial values = X0 
  X[,1,] <- X0 
  call_payoff[,1,] <- put_payoff[,1,] <- 0
  
  B <- t(chol(Sigma)) 
  dt <- t/ N 
  muMat <- matrix(rep(mu -0.5 * diag(Sigma) * diag(Sigma), M), 
                  ncol = d, byrow = T) 
  
  ## matrix for deltas 
  deltas <- call_put <- array(NA, dim = c(N + 1, d)) 
  
  ## Run the simulation 
  for(i in 1:N){
    Z <- matrix(rnorm(d * M), ncol = M) 
    
    X[,i + 1,] <- X[,i,] * exp(muMat * dt + sqrt(dt) * t(B %*% Z)) 
    Zmat[,i + 1,] <- Z
    
    ## Finite Differences 
    Xh[,i + 1,] <- (X[,i,] + 0.01) * exp(muMat * dt + sqrt(dt) * t(B %*% Z)) 
    
    for(j in 1:d){
      ## We want to find the deltas for each of the paths 
      ##take each individual stock 
      ind_stock_X <- X[,i + 1,j] 
      ind_stock_Xh <- Xh[,i + 1, j] 
      
      call_delta <- Delta(eput, ind_stock_X, ind_stock_Xh) 
      put_delta <- Delta(ecall, ind_stock_X, ind_stock_Xh) 
      call_put[i,j] <- ifelse(call_delta > put_delta, 'call', 'put') 
      deltas[i,j] <- pmax(call_delta, put_delta) 
      
      print(call_delta) 
      
    }
    
    
    
    
    
  }
  
  
  
  return(list('X' = X, 'Z' = Z, 'Delta' = deltas, 'Option_Type' = call_put)) 
  
}