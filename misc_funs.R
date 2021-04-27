
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
    add_column('Moments' = c('mean', 'variance', 'skewness', 'kurtosis'), 
               .before = 'SYY')
  
  
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

Delta <- function(option, S, Sh){
  ## Find the delta values 
  f0 <- option(S) 
  f0h <- option(Sh) 
  fd <- exp(-0.05 * t) * mean(abs(f0h - f0) / 0.01) 
  return(fd) 
}





