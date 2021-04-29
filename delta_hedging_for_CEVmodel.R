S0 <- 49
K <- 50
r <- 0.05
alpha <- 0.8
sigma <- 0.20*S0^(1-alpha)
t <- 20/52
mu <- 0.13
n <- 1



# 1. simulate the paths
M <- 10000
N <- 4

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

delta.hedge <- function(M,N,S0,K,r,sigma,t,mu){
  print(N)
  
  d1 <- (log(S0/K) + (r + sigma*sigma/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  bls <- S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  
  # Plain vanilla call payoff function
  f <- function(S,K){
    f <- pmax(S-K,0)
  }
  
  h <- 0.1
  
  delta <- function(M,n,t,r,S0,K,sigma,ss=1){
    set.seed(ss)
    #ST <- S0*exp((r - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(M))
    ST <- myCEV(M,N,r,sigma,t,S0,alpha)$CEV[,N+1]
    set.seed(ss)
    #STh <- (S0+h)*exp((r - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(M))
    STh <-myCEV(M,N,r,sigma,t,S0+h,alpha)$CEV[,N+1]
    f0 <- f(ST,K)
    f0h <- f(STh,K)
    fd <- exp(-r*t)*mean((f0h - f0) / h)
  }
  
  # Simulate the paths and deltas:
  X <- deltas <- matrix(NA,ncol=N+1,nrow=M)
  X[,1] <- S0
  dt <- t/N
  for (i in 1:N){
    X[,i+1] <- X[,i]+ r*X[,i]*dt + 
      sigma*X[,i]^alpha*sqrt(dt)*rnorm(M)
    
    ttm <- t - dt*(i-1)
    deltas[,i] <- delta(M,n,ttm,r,X[,i],K,sigma)
  }
  
  # Fill in terminal deltas (1/0):
  deltas[,N+1] <- delta(M,n,0,r,X[,N+1],K,sigma)
  
  # Generate a matrix of positions:
  CF <- matrix(NA,ncol=N+1,nrow=M)
  CF[,1] <- -deltas[,1]*S0
  for (i in 1:(N-1)){
    CF[,i+1] <- -1*(deltas[,i+1] - deltas[,i])*X[,i+1]
  }
  
  IN <- which(X[,N+1] > K)
  CF[IN,N+1] <- K - (1-deltas[IN,N])*X[IN,N+1]
  CF[-IN,N+1] <- deltas[-IN,N]*X[-IN,N+1]
  
  # 3. sum the costs:
  disc <- matrix(exp(-r*seq(0,t,length=N+1)),ncol=1)
  PV <- (CF * 0.99)%*%disc
  
  # compute performace
  H.perf <- sqrt(var(PV))/bls
  
  print(sqrt(var(PV)))
  print(bls) 
  print(H.perf) 
  outlist <- list("H.perf"=H.perf,"PV"=PV,"BLS"=BLS)
  return(outlist)
}



N <- c(4,5,10,20,40,80)
H <- c(NA)
PV <- c(NA)
for (j in 1:50){
  print(j) 
  alpha = 0.8
  tmp <- delta.hedge(M,52,S0,K,r,sigma, round(j / 52),mu)
  H[j] <- tmp$H.perf
  PV[j] <- mean(tmp$PV)
}

print(H)
print(PV)
print(tmp$H.perf)
H
