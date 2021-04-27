
## Annualize Returns formula 
Annualize <- function(x){
  rets <- ((1 + x) ^ 365) - 1 
  return(rets) 
}

## Formulas to find moments 
Moments <- function(x){
  ### Find the moments 
  data_no_date <- x %>% 
    select(-Date)
  summary <- data_no_date %>% 
    ## Mean 
    summarize_all(mean) %>% 
    ## Variance
    bind_rows(data_no_date %>% summarize_all(sd)) %>% 
    ## Skewness
    bind_rows(data_no_date %>% summarize_all(moments::skewness)) %>% 
    ## Kurtosis
    bind_rows(data_no_date %>% summarize_all(moments::kurtosis)) %>% 
    ## 
    add_column('Moments' = c('mean', 'variance', 'skewness', 'kurtosis'))
  
  
  return(summary) 
}

## Functions for returns 
Returns <- function(prices){
  
  ## Formula for returns on stocks
  Rets_Formula <- function(x){
    rets <- (x - lag(x)) / lag(x) 
    return(rets) 
  }
  
  ## Prices to returns function 
  rets <- prices %>% select(-RF) %>% 
    ## Apply returns, function to adjusted prices 
    mutate_at(vars(-Date), Rets_Formula) %>% 
    drop_na() 
  
  return(rets) 
}

##Return single moment from Moments df
Single_Moment <- function(moments, name){
  return(
    moments %>% 
      filter(Moments == name) %>% 
      select(-Moments) 
  )
}

## Call option 
ecall <- function(S){
  c <- pmax(S - K, 0) 
  return(c) 
}

## put option 
eput <- function(S){
  p <- pmax(K - S, 0) 
  return(p) 
}

Delta <- function(option, S, Sh, h, t){
  ## Find the delta values 
  f0 <- option(S) 
  f0h <- option(Sh) 
  fd <-  mean(exp(-0.05 * t) * abs(f0h - f0) / h) 

  return(fd) 
}

## function to subtract costs and discount. 
Cost_Fun <- function(X, N, t, fee, discount){
  ## first subtract the cost of the transaction 
  ## Then discount the net present value 
  price <- X * (1 - fee)  
  
  price <- price / (1 + discount)^(N * t / 260) ## time (days) * number of periods / years 
  return(price) 
}

BLS <- function(M,N,S0,K,r,sigma,t,mu){
  print(N)
  
  d1 <- (log(S0/K) + (r + sigma*sigma/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  BLS <- S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  return(BLS) 
}




