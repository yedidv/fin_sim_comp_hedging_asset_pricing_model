
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

## Black Scholes Pricing for options 
Black_Scholes_Price <- function(S0, K, r, sigma, t){
  d1 <- (log(S0 / K) + (r + diag(sigma)^2 / 2) * t) / (diag(sigma) * sqrt(t))
  d2 <- d1 - diag(sigma) * sqrt(t) 
  bls <- S0 * pnorm(d1) - K * exp(-r * t) * pnorm(d2) 
  return(bls) 
}

## Delta Function 
Delta <- function(S,K,r,t,sigma){
  d <- pnorm((log(S/K) + 
                (r + sigma*sigma/2)*t)/(sigma*sqrt(t)))
  return(d)
}


