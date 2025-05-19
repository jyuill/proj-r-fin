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

# load functions
source("functions.R")

# Get data
# portfolio info
## set up google auth
if(Sys.getenv("GOOGLE_AUTH_JSON")!=""){
  gs4_auth(path = Sys.getenv("GOOGLE_AUTH_JSON"))
} else {
  stop("Please set up google auth json file")
}

gsheet <- 'https://docs.google.com/spreadsheets/d/1DRW5n6s6UtDAq3-kg-JTD4Yd2_24QfjAAxSU7-itkEE/edit?usp=sharing'
sheet_item <- 'crypto-mom'

portfolio <- read_sheet(ss=gsheet, sheet=sheet_item, skip=2)

## get latest prices - convert to Cdn$ if nec
prices <- data.frame()
for(c in 1:nrow(portfolio)){
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

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

}
