rm(list = ls()) 
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(moments) 
#setwd("F:/Spring/Financial computation and simulation")
source('asset_price_model.r') 
source('misc_funs.r') 

set.seed(2654)
## Read the data #### 
n_stocks <- 1
prices <- Read_Data(n_stocks)
prices %>% head() 

## Calculate Returns 
rets <- Returns(prices)

## Look at the moments for the prices and the returns 
price_moms <- Moments(prices) 
price_moms
rets_moms <- Moments(rets) 
rets_moms

mean_rets <- Single_Moment(rets_moms, 'mean') %>% 
  t()
mu<- as.numeric(matrix(Annualize(mean_rets), ncol  = 1))  
vol <- as.numeric(Annualize(var(rets %>% select(-Date), use = 'complete.obs')))
S0 <- as.numeric(prices %>% select(-Date, -RF) %>% tail(1)%>% t())
K <- as.numeric(prices %>% select(-Date, -RF) %>% tail(1) %>% t()) +5
r <- 0.05
alpha <- 1
sigma <- vol*S0^(1-alpha)
t <- 1
n <- 1



# 1. simulate the paths
M <- 100
N <- 40

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

delta.hedge <- function(M,N,S0,K,r,sigma,t,mu,call=1){
  print(N)
  
  
  if (call == 1){
    d1 <- (log(S0/K) + (r + vol*vol/2)*t)/(vol*sqrt(t))
    d2 <- d1 - vol*sqrt(t)
    BLS <- S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
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
    X[,i+1] <- X[,i]+ mu*X[,i]*dt + 
      sigma*X[,i]^alpha*sqrt(dt)*rnorm(M)
    
    ttm <- t - dt*(i-1)
    for (j in 1:M) {
      deltas[j,i] <- delta(M,n,ttm,r,X[j,i],K,sigma)
    }
    
  }
  
  # Fill in terminal deltas (1/0):
  for (j in 1:M) {
    deltas[j,N+1] <- delta(M,n,0,r,X[j,N+1],K,sigma)
  }

  
  # Generate a matrix of positions:
  CF <- matrix(NA,ncol=N+1,nrow=M)
  CF[,1] <- -deltas[,1]*S0
  for (j in 1:M) {
    for (i in 1:(N-1)){
      # transaction cost equals to 1% of the trading value
      #if(deltas[j,i+1] < deltas[j,i]){
      CF[j,i+1] <- -1*(deltas[j,i+1] - deltas[j,i])*X[j,i]
        #CF[j,i+1] <- (-1*(deltas[j,i+1] - deltas[j,i])*X[j,i])*(1+0.01)
      #} else {
        #CF[j,i+1] <- (-1*(deltas[j,i+1] - deltas[j,i])*X[j,i+1])*(1-0.01)
      #}
      
    }
    
  }

  IN <- which(X[,N+1] > K)
  CF[IN,N+1] <- K - ((1-deltas[IN,N])*X[IN,N+1])
  CF[-IN,N+1] <- deltas[-IN,N]*X[-IN,N+1]
  
  # 3. sum the costs:
  disc <- matrix(exp(-r*seq(0,t,length=N+1)),ncol=1)
  PV <- CF%*%disc
  
  # compute performace
  H.perf <- sqrt(var(PV))/BLS
  outlist <- list("H.perf"=H.perf,"PV"=PV,"BLS"=BLS)
  return(outlist)
  }
  else{
    d1 <- (log(S0/K) + (r + vol*vol/2)*t)/(vol*sqrt(t))
    d2 <- d1 - vol*sqrt(t)
    BLS <- K*exp(-r*t)*pnorm(-d2) - S0*pnorm(-d1)
    # Plain vanilla put payoff function
    f <- function(S,K){
      f <- pmax(K-S,0)
    }
    
    h <- 0.1
    
    delta <- function(M,n,t,r,S0,K,sigma,ss=1){
      set.seed(ss)
      #ST <- S0*exp((r - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(M))
      ST <- myCEV(M,N,r,sigma,t,S0,alpha)$CEV[,n+1]
      set.seed(ss)
      #STh <- (S0+h)*exp((r - 0.5*sigma^2)*t+sigma*sqrt(t)*rnorm(M))
      STh <-myCEV(M,N,r,sigma,t,S0+h,alpha)$CEV[,n+1]
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
      for (j in 1:M) {
        deltas[j,i] <- delta(M,n,ttm,r,X[j,i],K,sigma)
      }
      
    }
    
    # Fill in terminal deltas (1/0):
    for (j in 1:M) {
      deltas[j,N+1] <- delta(M,n,0,r,X[j,N+1],K,sigma)
    }
    
    
    # Generate a matrix of positions:
    CF <- matrix(NA,ncol=N+1,nrow=M)
    CF[,1] <- -deltas[,1]*S0
    for (j in 1:M) {
      for (i in 1:(N-1)){
        # transaction cost equals to 1% of the trading value
        #if(deltas[j,i+1] < deltas[j,i]){
        CF[j,i+1] <- -1*(deltas[j,i+1] - deltas[j,i])*X[j,i]
        #CF[j,i+1] <- (-1*(deltas[j,i+1] - deltas[j,i])*X[j,i])*(1+0.01)
        #} else {
        #CF[j,i+1] <- (-1*(deltas[j,i+1] - deltas[j,i])*X[j,i+1])*(1-0.01)
        #}
        
      }
      
    }
    
    IN <- which(X[,N+1] < K)
    CF[IN,N+1] <- -(((-1)-deltas[IN,N])*X[IN,N+1]) - K 
    CF[-IN,N+1] <- deltas[-IN,N]*X[-IN,N+1]
    
    # 3. sum the costs:
    disc <- matrix(exp(-r*seq(0,t,length=N+1)),ncol=1)
    PV <- CF%*%disc
    
    # compute performace
    H.perf <- sqrt(var(PV))/BLS
    outlist <- list("H.perf"=H.perf,"PV"=PV,"BLS"=BLS)
    return(outlist)
  }
}



#N <- c(4,5,10,20,40,80)
N <- c(4,5)
H <- c(NA)
PV <- c(NA)
for (j in 1:length(N)){
  tmp <- delta.hedge(M,N[j],S0,K,r,sigma,t,mu,call = 0)
  H[j] <- tmp$H.perf
  PV[j] <- mean(tmp$PV)
}

print(H)
print(PV)
print(tmp$BLS)
