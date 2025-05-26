#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(DT)
library(plotly)
library(RColorBrewer)
library(PerformanceAnalytics) 
library(googlesheets4)
library(quantmod) ## for prices; includes zoo (which includes xts)
library(dygraphs)
library(here)

# load functions ----
# running locally
#source(here("crypto-mom/functions.R"))
source("functions.R")

# Get data ----
## portfolio info ----
## set up google auth
if(Sys.getenv("GOOGLE_AUTH_JSON")!=""){
  gs4_auth(path = Sys.getenv("GOOGLE_AUTH_JSON"))
} else {
  stop("Please set up google auth json file")
}

gsheet <- 'https://docs.google.com/spreadsheets/d/1DRW5n6s6UtDAq3-kg-JTD4Yd2_24QfjAAxSU7-itkEE/edit?usp=sharing'
sheet_item <- 'crypto-mom'

portfolio <- read_sheet(ss=gsheet, sheet=sheet_item, skip=2)

## get prices ----
## get latest prices - convert to Cdn$ if nec
prices <- data.frame()
for(c in 1:nrow(portfolio)){
  # in testing, needed to reload function each time -> hopefully not in prod
  source(here("crypto-mom/functions.R"))
  coin <- portfolio$coin[c]
  sym <- portfolio$symbol[c]
  date_start <- portfolio$date_purch[c]
  price <- price(coin, sym, date_start)
  #prices <- bind_cols(prices, price)
  #prices <- merge(prices, price, by="date", all=TRUE)
  if(nrow(prices)==0) {
    prices <- price
  } else {
    prices <- merge(prices, price, by="date", all=TRUE)
    #prices <- full_join(prices, price, by="date")
  }
}

## combine latest prices with portfolio info
prices_latest <- prices %>% filter(date==max(date))
prices_latest_long <- prices_latest %>%
  pivot_longer(cols = -date, names_to = "coin", values_to = "price")

# calculate returns
portfolio_latest <- full_join(portfolio, prices_latest_long, by = "coin") %>%
  mutate(date = as.Date(date)) %>%
  select(coin, symbol, date_purch, amt_purch, price_purch, total_invest, date, price) %>%
  mutate(total_value = price * amt_purch,
         gain = total_value - total_invest,
         roi = gain / total_invest)
# calc summary of total returns
portfolio_ttl_return <- portfolio_latest %>% group_by(date) %>%
  summarize(
    total_invest = sum(total_invest, na.rm = TRUE),
    total_value = sum(total_value, na.rm = TRUE),
    total_gain = sum(gain, na.rm = TRUE),
    total_roi = total_gain / total_invest
  ) %>%
  mutate(date = as.Date(date))

# Define server logic required to draw a histogram
function(input, output, session) {
    # data
  
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- prices[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Price distribution',
             main = 'Prices')

    })
    
    output$portfolioTotal <- renderDataTable({
        datatable(portfolio_ttl_return, 
                  class = "display compact",
                  options = list(dom = 't', # table only - no search, pagination, etc.
                                 ordering = FALSE,
                                 autoWidth = TRUE),
                  rownames = FALSE # hide row names
                  ) %>%
        formatCurrency("total_invest", "$", digits = 0) %>%
        formatCurrency("total_value", "$", digits = 0) %>%
        formatCurrency("total_gain", "$", digits = 0) %>%
        formatPercentage("total_roi", digits = 1)
    })
    
    output$portfolioLatest <- renderDataTable({
        datatable(portfolio_latest[, c("coin", 
                                       "amt_purch", "price_purch", 
                                       "price", 
                                       "total_value", "gain", "roi")], 
                  class = "display hover compact",
                  options = list(dom = 't', 
                                 autoWidth = TRUE),
                  rownames = FALSE) %>%
        formatRound("amt_purch", digits = 2) %>%
        formatCurrency("price_purch", "$", digits = 0) %>%
        formatCurrency("price", "$", digits = 0) %>%
        formatCurrency("total_value", "$", digits = 0) %>%
        formatCurrency("gain", "$", digits = 0) %>%
        formatPercentage("roi", digits = 1)
    })

}
