## Exploring Cryptocurrency data
## using crypto pkg
## https://cran.r-project.org/web/packages/crypto/readme/README.html
## data is scraped from:
## https://coinmarketcap.com/ 

library(tidyverse)
library(crypto)

## examples from link above
# Retrieve crypto market history for all-to-n coins
data_hist_top <- crypto_history(limit=5)

# Retrieve crypto market history for specific coin
data_hist_specific <- crypto_history("btc") ## get bitcoin data

# Get list of coins and rank
cryptos_all <- crypto_list()

# Retrieve current crypto market details
prices_current <- crypto_prices()

# Convert and/or summarise market history into xts object
## not clear on advantage
price_ts_top <- crypto_xts(data_hist_top, "week")

## NOT TESTED BY JY YET
# Get timeseries market data for token for displaying in charts
price_ts_specific <- crypto_timeseries('bitcoin')
## chart
ggplot(price_ts_specific, aes(x=timestamp, y=price_usd))+geom_line()

# Get timeseries global market data for all coins or alt coins for displaying in charts
#price_ts_all <- crypto_global_market()