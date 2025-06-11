# functions for crypto-mom

# get prices
price_fetch <- function(coin, symbol, date_start){
  symbol_price <- getSymbols(Symbols=symbol, from = as.Date(date_start), src="yahoo", auto.assign=FALSE)
  ## match dates with cost data
  #dt_start <- date_start # incorporated into getSymbols
  ## GENERIC object name for easy re-use
  symbol_close <- paste0(symbol,".Close")
  price <- symbol_price[,symbol_close] # incorporated into getSymbols
  ## convert ta data frame and combine with cost data
  price_df <- as.data.frame(price)
  price_df$date <- date(row.names(price_df))
  row.names(price_df) <- seq(1:nrow(price_df))
  price_df <- price_df %>% select(date, 1:ncol(price_df)-1)
  names(price_df)[2] <- coin
  return(price_df)
}