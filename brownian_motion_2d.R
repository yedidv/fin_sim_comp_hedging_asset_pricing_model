

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

myCEV <- function(M,N,r,sigma,t,S0,alpha){
  S.Euler <- matrix(NA,ncol=N+1,nrow=M)
  S.CEV <- matrix(NA,ncol=N+1,nrow=M)
  S.Euler[,1] <- S0
  S.CEV[,1] <- S0
  dt <- t/N
  sqdt <- sqrt(dt)
  
  for (i in 1:N){
    # use a common Z to compare methods:
    Z <- matrix(rnorm(M),ncol=1)
    
    # GBM:
    S.Euler[,i+1] <- S.Euler[,i] + r*S.Euler[,i]*dt + 
      sigma*S.Euler[,i]*sqdt*Z
    
    # CEV:
    S.CEV[,i+1] <- S.CEV[,i] + r*S.CEV[,i]*dt + 
      sigma*S.CEV[,i]^alpha*sqdt*Z
  }
  S.out <- list("GBM"=S.Euler,"CEV"=S.CEV)
  return(S.out)
}




