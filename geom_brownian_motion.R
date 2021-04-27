Geom_Brownian <- function(M, N, d, t, mu, X0, Sigma){
  X <- array(NA, dim = c(M, N + 1, d) )
  Xh <- Zmat <- CV <- call_payoff <- put_payoff<- X
  
  
  
  ## Set initial values = X0 
  X[,1,] <- X0 
  call_payoff[,1,] <- put_payoff[,1,] <- 0
  
  B <- t(chol(Sigma)) 
  dt <- t/ N 
  muMat <- matrix(rep(mu -0.5 * diag(Sigma) * diag(Sigma), M), 
                  ncol = d, byrow = T) 
  
  ## matrix for deltas 
  deltas <- call_put <- array(NA, dim = c(N + 1, d)) 
  
  h <- 0.1
  
  ## Run the simulation 
  for(i in 1:N){
    Z <- matrix(rnorm(d * M), ncol = M) 
    

    X[,i + 1,] <- X[,i,] * exp(muMat * dt + sqrt(dt) * t(B %*% Z)) 
    Zmat[,i + 1,] <- Z
    
    ## Finite Differences 
    Xh[,i + 1,] <- (X[,i,] + h) * exp(muMat * dt + sqrt(dt) * t(B %*% Z)) 
    
    ## Subtract costs and discount rate 
    #X[,i + 1,] <- Cost_Fun(X[,i + 1,], i, t, 0.01, 0.1)
    #Xh[,i + 1,] <- Cost_Fun(Xh[,i + 1,], i, t, 0.01, 0.1)
    
    for(j in 1:d){
      ## We want to find the deltas for each of the paths 
      ##take each individual stock 
      ind_stock_X <- X[,i + 1,j] 
      ind_stock_Xh <- Xh[,i + 1, j] 

      ## Find the deltas for put and call options 
      ## We take the max delta for eacH time step for each option 
      ## create dataframe containing whether or not it's reccomended 
      ## to put or call at that period in time. 
      call_delta <- Delta(eput, ind_stock_X, ind_stock_Xh, h, t) 
      put_delta <- Delta(ecall, ind_stock_X, ind_stock_Xh, h, t) 
      call_put[i+1,j] <- ifelse(call_delta > put_delta, 'call', 'put') 
      deltas[i+1,j] <- pmax(call_delta, put_delta) 
      

    }
    call_put[1,] <- 0 
    deltas[1,] <- 0

  }
  return(list('X' = X, 'Z' = Z, 'Delta' = deltas, 'Option_Type' = call_put)) 
}
