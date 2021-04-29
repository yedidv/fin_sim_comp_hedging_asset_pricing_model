rm(list = ls()) 
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(moments) 
setwd("~/Desktop/msba/spring2021/fin_sim_comp_hedging_asset_pricing_model")
source('asset_price_model.r') 
source('misc_funs.r') 
source('brownian_motion_2d.r') 

## Read the data 
n_stocks <- 1
## Annualize stock prices 
prices <- Read_Data(n_stocks) 
prices

## Convert to returns
rets <- Returns(prices) 

## Find the moments of the returns 
## So we can have the variance, skewness, kurtosis
rets_moms <- Moments(rets) 
rets_moms

## Find variance and mean
sigma <- as.numeric(var(rets %>% select(-Date), use = 'complete.obs')[1])
mu <- as.numeric(Single_Moment(rets_moms, 'mean')[1] ) 
rf <- as.numeric((prices %>% select(RF) %>% drop_na() %>% tail(1))[1])  
rf
t <- 2 / 52 ## initial time period (in years) 
M <- 1000 ## number of paths 
N <- 52 ## numer time steps 


## Strike price listed as last value of the stock 
K <- as.numeric((prices %>% select(-Date, -RF) %>% tail(1))[1])
S0 <- K ## initial price is going to be labeled the same as strike price

## GBM Model 
gbm_model <- Brown_Motion(M, N, t, S0, mu, sigma) 
deltas <- gbm_model$Deltas
X <- gbm_model$X 
Price_Path_Plot(M, N, gbm_model$X) 
Log_Ret_Hist(gbm_model$X) 



dgbm <- Delta_Perf(M, N, deltas, X) 
dgbm$H.perf
dgbm$CF
