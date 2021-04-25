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
M <- 100 
N <- 252 
d <- n_stocks 
t <- 1 
## Unable to start at appropriate starting prices 
X0 <- prices %>% select(-Date) %>% tail(1) 
## Only works when start returns are at a arbritrary number 
X0 <- prices %>% select(-Date) %>% tail(1) %>% t() %>% mean() 

gmd_model <- Geom_Brownian(M, N, n_stocks, t, mu, X0, Sigma) 


## Strike Price listed as mean value of all stocks (?) 
K <- Single_Moment(price_moms, 'mean') %>% 
            t() %>% 
            mean() 








