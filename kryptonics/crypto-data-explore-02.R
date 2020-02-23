## Exploring Cryptocurrency data
## using crypto pkg
## https://cran.r-project.org/web/packages/crypto/readme/README.html
## data is scraped from:
## https://coinmarketcap.com/ 

library(tidyverse)
library(crypto)

## examples from link above
## Retrieve crypto market history for top n coins
data_hist_top <- crypto_history(limit=5)

## plot closing prices for all
ggplot(data_hist_top, aes(x=date, y=close, color=symbol))+geom_line()

## plot specific prices
crypto_sel <- "ETH"
data_hist_select <- data_hist_top %>% filter(symbol==crypto_sel)
ggplot(data_hist_select, aes(x=date, y=close, color=symbol))+geom_line()

## check correlation in the last 2 yrs
dt_start <- max(data_hist_top$date)-(365*2)
data_hist_top_rec <- data_hist_top %>% filter(date>=dt_start)
## select coins
data_hist_top_rec_sel <- data_hist_top_rec %>% 
  filter(symbol=='BTC'|symbol=='ETH') %>%
  select(symbol, date, close)
## spread data to get coins in column by date
data_sel_rec_spread <- data_hist_top_rec_sel %>% spread(symbol, close)

## correlation chart
ggplot(data_sel_rec_spread, aes(x=BTC, y=ETH))+geom_point()
