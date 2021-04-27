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
t <- 1
r <- 0.05
## Strike Price listed as Last value of all stocks 
K <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 
## Unable to start at appropriate starting prices 
X0 <- prices %>% select(-Date) %>% tail(1) 
## Only works when start returns are at a arbritrary number 
X0 <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 

## Brownian Motion model. 
gmd_model <- Geom_Brownian(M, N, n_stocks, t, mu, X0, Sigma) 

X <- gmd_model$X[,,2] 
deltas <- gmd_model$Delta[,,2]


# Generate a matrix of positions:
CF <- matrix(NA,ncol=N+1,nrow=M)
CF[,1] <- -deltas[,1]*X0
for (i in 1:(N-1)){
  CF[,i+1] <- -1*(deltas[,i+1] - deltas[,i])*X[,i+1]
}

IN <- which(X[,N+1] > K)
CF[IN,N+1] <- K - (1-deltas[IN,N])*X[IN,N+1]
CF[-IN,N+1] <- deltas[-IN,N]*X[-IN,N+1]
CF
# 3. sum the costs:
disc <- matrix(exp(-r*seq(0,t,length=N+1)),ncol=1)
PV <- CF%*%disc
PV
bls <- BLS(M, N, X0, K, r, diag(Sigma)[2], t, mu )

bls
# compute performace
H.perf <- sqrt(var(PV))/bls
print(H.perf)





