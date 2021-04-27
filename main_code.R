rm(list = ls()) 
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(moments) 
setwd("~/Desktop/msba/spring2021/fin_sim_comp_hedging_asset_pricing_model")
source('asset_price_model.r') 
source('misc_funs.r') 
source('geom_brownian_motion.r') 



## Read the data #### 
n_stocks <- 2
prices <- Read_Data(n_stocks)
prices %>% head() 


## Calculate Returns 
rets <- Returns(prices)

## Look at the moments for the prices and the returns 
price_moms <- Moments(prices) 
price_moms
rets_moms <- Moments(rets) 
rets_moms

## GMD Model #### 
mean_rets <- Single_Moment(rets_moms, 'mean') %>% 
  t()
mu<- matrix(mean_rets, ncol  = 1)  
Sigma <- var(rets %>% select(-Date), use = 'complete.obs') 
rho <- cor(rets %>% select(-Date), use = 'complete.obs') 
sigma <- var(rets %>% select(-Date), use = 'complete.obs') 
M <- 5 ## Number of paths 
N <- 7 ## number of time steps 
d <- n_stocks 

## Strike Price listed as Last value of all stocks 
K <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 
## Unable to start at appropriate starting prices 
X0 <- prices %>% select(-Date) %>% tail(1) 
## Only works when start returns are at a arbritrary number 
X0 <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 

## Brownian Motion model. 
gmd_model <- Geom_Brownian(M, N, n_stocks, 30, mu, X0, Sigma) 

stock_a <- gmd_model$X[,,2] 
deltas_a <- gmd_model$Delta[,2]
deltas_a



## Matrix of Positions
CF <- matrix(NA, ncol=N+1,nrow=M) 

CF[,1] <- deltas_a[1] * stock_a[,1] 
for(i in 1:N){
  CF[,i + 1] <- -1*(deltas_a[i + 1] - deltas_a[i]) * stock_a[,i + 1] 
}
CF

deltas_a[1] * stock_a[,1]

dim((abs(var(CF))) ^ 0.5)
dim(stock_a) 
dim(CF) 
dim(var(CF)) 

BLS <- function(M,N,S0,K,r,sigma,t,mu){
  print(N)
  
  d1 <- (log(S0/K) + (r + sigma*sigma/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  BLS <- S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  return(BLS) 
}

