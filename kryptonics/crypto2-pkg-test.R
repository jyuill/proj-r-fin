## Exploring Cryptocurrency data
## using crypto2 pkg
## 
## data is scraped from:
## https://coinmarketcap.com/ 

library(tidyverse)
library(crypto2)
library(lubridate)

## examples from link above
## Retrieve crypto market history for selected coins
coins <- crypto_list() %>% filter(symbol=='BTC'|symbol=='ETH'|symbol=='LTC'|symbol=='ADA')
data_hist <- crypto_history(coin_list=coins, start_date='20160101')
data_hist$time_open <- date(data_hist$time_open)

## plot closing prices for all
ggplot(data_hist, aes(x=time_open, y=close, color=symbol))+geom_line()

## plot specific prices
crypto_sel <- "ETH"
data_hist_select <- data_hist %>% filter(symbol==crypto_sel)
ggplot(data_hist_select, aes(x=time_open, y=close, color=symbol))+geom_line()

## check correlation in the last 2 yrs
dt_start <- max(data_hist$time_open)-(365*2)
data_hist_rec <- data_hist %>% filter(time_open>=dt_start)
## select coins
data_hist_rec_sel <- data_hist_rec %>% 
  filter(symbol=='BTC'|symbol=='ETH') %>%
  select(symbol, time_open, close)
## spread data to get coins in column by date
data_sel_rec_spread <- data_hist_rec_sel %>% spread(symbol, close)

## correlation chart
ggplot(data_sel_rec_spread, aes(x=BTC, y=ETH))+geom_point()
