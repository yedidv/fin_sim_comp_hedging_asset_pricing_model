rm(list = ls()) 
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(moments) 
setwd("ba/fcs/project/fin_sim_comp_hedging_asset_pricing_model/fin_sim_comp_hedging_asset_pricing_model-main")
source('asset_price_model.r') 
source('misc_funs.r') 
source('stochastic_volatility.r')
source('geom_brownian_motion.r') 




## Read the data #### 
n_stocks <- 6
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
M <- 10 ## Number of paths 
N <- 13 ## number of time steps 
d <- n_stocks 
t <- 1 
## Strike Price listed as Last value of all stocks 
K <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 
## Unable to start at appropriate starting prices 
X0 <- prices %>% select(-Date) %>% tail(1) 
## Only works when start returns are at a arbritrary number 
X0 <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 

gmd_model <- Geom_Brownian(M, N, n_stocks, t, mu, X0, Sigma) 

gmd_model$Delta
gmd_model$Option_Type
gmd_model$X

snp <- getSymbols('^GSPC', src = 'yahoo', from = as.Date('2010-1-1'), 
                  to = as.Date('2021-1-1'), auto.assign = F) 
snp
snp <- as.data.frame(snp)

snp.ret <- diff(snp[,6])/lag(snp[,6])
snp.ret
rets <- as.data.frame(rets)
prices <- as.data.frame(prices)
a <- b <- S0 <- V0 <- volvol <- array(NA, dim=c(d))
delts <- array(NA, dim=c(d,M))
for(i in 1:d){
  result <- lm(rets[,i+1]~snp.ret[2:2769])
  a[i] <- result$coefficients[1]
  b[i] <- result$coefficients[2]
  V0[i] <- sd(rets[,i+1])
  S0[i] <- prices[[i+1]][2768]
  volvol[i] <- V0[i]*V0[i]
  results<- Stochastic.deltas(M, N, d, t, mu[i], a[i], b[i], volvol[i], S0[i], V0[i])
  delts[i,] <- results$Deltas
  
}

delts
