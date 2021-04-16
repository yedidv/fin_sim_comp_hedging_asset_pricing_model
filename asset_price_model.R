rm(list = ls()) 
setwd("~/Desktop/msba/spring2021/Fin_Computation_Simulation/hedging_asset_pricing_model")
library(tidyverse) 
library(Matrix) 
library(plotly) 
library(rvest)
library(quantmod) 
library(lubridate) 


#### Read in Data #### 
Read_Data <- function(random_sample, 
                      start_date = as.Date('2010-1-1'), 
                      end_date = as.Date('2021-1-1')){
  ## Read Ticker Symbols for all companies in SP500 
  url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies' %>% 
    read_html() %>% 
    html_table(header = T, fill = T) %>% 
    .[[1]] %>% 
    as_tibble()
  
  ## Subset of the tickers. Get random sample 
  ticker_sample <- url %>% 
    ## Select Ticker Symbols
    select(Symbol) %>% 
    ## Random sample of tickers given the size we want 
    sample_n(size = random_sample)
  
  ## Empty dataframe for final prices 
  prices <- tibble(
    Date = seq.Date(from= start_date, 
                    to= end_date, 
                    by="day")
  ) 
  for(i in (ticker_sample$Symbol)){
    ## For each ticker symbol, bring in quantmod data 
    ## and join with dataframe
    
    tic <- getSymbols(i, src = 'yahoo', start = start_date, 
                      end = end_date, auto.assign = F) 
    ## Extract the date 
    date <- index(tic) 
    tic <- as_tibble(tic) ## Convert to tibble 
    
    
    names(tic) <- c('Open', 'High', 
                    'Low', 'Close',
                    'Volume', 
                    'Adjusted') ## Rename columns 
    
    tic <- tic %>% 
      add_column(Date = as.Date(date)) %>% ## Add the date back to tibble
      select(Date, Adjusted) ## we only care about the prices and the date 
    
    ## Rename price to the ticker name 
    names(tic) <- c('Date', i)
    
    
    ## inner join on the overall dataframe 
    prices <- tic %>% inner_join(prices, by = 'Date') 
    
  }
  
  return(prices) 
  
}

## random_sample is how many tickers we want from the sp500 
prices <- Read_Data(random_sample = 3)  
prices %>% head() 







