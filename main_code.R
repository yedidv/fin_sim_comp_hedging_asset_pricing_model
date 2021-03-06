rm(list = ls()) 
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(moments) 
setwd("~/Desktop/msba/spring2021/fin_sim_comp_hedging_asset_pricing_model")
source('asset_price_model.r') 
source('misc_funs.r') 
source('models.r') 

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
r <- rf 
t <- 1 ## initial time period (in years) 
M <- 100 ## number of paths 
N <- 52 ## numer time steps 


## Strike price listed as last value of the stock 
K <- as.numeric((prices %>% select(-Date, -RF) %>% tail(1))[1])
S0 <- K ## initial price is going to be labeled the same as strike price



## GBM Model 
gbm_hedge <- Hedge_Performance(Brown_Motion, M, N, S0, mu, sigma) 
gbm_hedge$Plot
gbm_hedge$PV
t <- gbm_hedge$Time
gbm_model <- Brown_Motion(M, N, t, S0, mu, sigma)
deltas <- gbm_model$Deltas
X <- gbm_model$X

gbm_hedge$Plot
gbm_price_path <- Price_Path_Plot(M, N, gbm_model$X)
gbm_price_path
gbm_log_rets <- Log_Ret_Hist(gbm_model$X)
gbm_log_rets
dgbm <- Delta_Perf(M, N, t, deltas, X)
dgbm$H.perf

## CEV Model
source('misc_funs.r') 
cev_model <- CEV(M, N, t, S0, mu, sigma, alpha = 1.2)
Price_Path_Plot(M, N, cev_model$X) 
hedge <- Hedge_Performance(CEV, M, N, S0, mu, sigma) 
hedge$Plot
cev_model <- myCEV(M, N, rf, sigma, t, S0, alpha = 0.8) 
Price_Path_Plot(M, N, cev_model$CEV) 
Log_Ret_Hist(cev_model$CEV) 

cev_model$S.CEV




source('models.r') 
cev_hedge <- Hedge_Performance(CEV, M, N, S0, mu, sigma) 

cev_hedge$Plot
cev_hedge$Time
cev_hedge$PV



dim(cev_model$Deltas) 




