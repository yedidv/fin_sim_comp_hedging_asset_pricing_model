
## Annualize Returns formula 
Annualize <- function(x){
  rets <- ((1 + x) ^ 365) - 1 
  return(rets) 
}

Log_Rets <- function(x){
  return(log(x + 1)) 
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
## Formula for returns on stocks
Rets_Formula <- function(x){
  rets <- (x - lag(x)) / lag(x) 
  return(rets) 
}
## Functions for returns 
Returns <- function(prices){
  

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

Delta <- function(option, S1, S2, h, t){
  ## Find the delta values 
  f1 <- option(S1) 
  f2 <- option(S2) 
  fd <-  mean(exp(-0.05 * t) * abs(f2 - f1) / (2 * h) )  

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

  
  d1 <- (log(S0/K) + (r + sigma*sigma/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  bls <- S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  return(bls) 
}


## Plot for potential stock prices 
Price_Path_Plot <- function(M, N, X){
  plt <- plot_ly(x = seq(1, N + 1), type = 'scatter', 
                 mode = 'lines', y = X[1,], 
                 showlegend = F) 
  for(i in 2:M){
    plt <- plt %>% 
      add_trace(y = X[i,], 
                type = 'scatter', mode = 'lines')
  }
  
  plt <- plt %>% layout(title = 'Potential Price Paths (Alpha = 0.8)') 
  return(plt) 
}

Log_Ret_Hist <- function(prices){
  ## Calculate log returns for all the returns 
  ## and plot the frequency histogram
  new_rets <- as_tibble(as_tibble((prices)) %>% t() ) %>% 
    mutate_all(Rets_Formula) %>% 
    mutate_all(Log_Rets) %>% 
    drop_na() %>% flatten()
  
  return(list('Plot' = plot_ly(x = new_rets, type = 'histogram') %>% 
           layout(title= 'Log Returns Distribution'), 
           'log_rets' = new_rets)
           ) 
}


## Delta Hedge 
Delta_Perf <- function(M, N, t,deltas, X){
  
  ## Black Scholes for pricing 
  bls <- BLS(M, N, S0, K, sigma, t, mu) 
  ## Generate matrix of positions 
  CF <- matrix(NA, ncol = N + 1, nrow = M) 
  CF[,1] <- -deltas[,1] * S0 
  for(i in 1:(N - 1)){
    CF[,i+1] <- -1*(deltas[,i+1] - deltas[,i])*X[,i+1]
  }
  
  IN <- which(X[,N+1] > K)
  CF[IN,N+1] <- K - (1-deltas[IN,N])*X[IN,N+1]
  CF[-IN,N+1] <- deltas[-IN,N]*X[-IN,N+1]
  
  # 3. sum the costs:
  disc <- matrix(exp(-rf*seq(0,t,length=N+1)),ncol=1)
  PV <- (CF * 0.99)%*%disc 
  
  # compute performace
  
  H.perf <- sqrt(var(PV))/bls
  # print(PV) 
  # print(sqrt(var(PV)))
  # print(bls) 
  print(H.perf) 
  return(list("H.perf"=H.perf,"PV"=PV,"BLS"=bls, 'CF' = CF))
}

Hedge_Performance <- function(price_model, M, N, S0, mu, sigma){
  
  perf_metric <- tibble(time = numeric(), 
                        perf = numeric(), 
                        P = numeric())
  for(i in 1:52){
    t <- i / 52 
    model <- price_model(M, N, t, S0, mu, sigma)
    deltas <- model$Deltas
    print(dim(deltas) ) 
    X <- model$X 
    dgbm <- Delta_Perf(M, N, t,deltas, X) 

    perf_metric <- perf_metric %>% add_row(time = i, 
                                           perf = dgbm$H.perf[1], 
                                           P = mean(dgbm$PV)) 
  }
  
  metric_plot <- plot_ly(perf_metric, x= ~time, y = ~perf, 
                         type = 'scatter', mode = 'lines', color = 'red') %>% 
    layout(title = 'Hedge Performance') 
  
  max_i <- max(perf_metric$time)
  return(list('Plot' = metric_plot, 'Time' = max_i, 'PV' = perf_metric)) 
  
} 


