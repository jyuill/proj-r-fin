#
# Crypto Mom 
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

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
library(shinythemes)

# settings
options(scipen = 10)
# chart size
ch_h <- '200px'
ch_hx <- '800px'
ch_w <- '90%'

# Define UI for application
# wrapping in function(request) at suggestion of chatGPT to control titlePanel when embedding in website
ui <- function(request) {
  # used to determine if app is embedded in website or not (when paired with url/?embed=true)
  query <- parseQueryString(request$QUERY_STRING)
  
fluidPage(

    # Application title - doesn't show when embedded in website
    if (is.null(query$embed)) {
      titlePanel("Crypto Mom! Investment Portfolio Status")    
    } ,
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
            checkboxGroupInput(inputId = "selected_coins",
                              label = "Select Coins",
                              choices = NULL # to be populated dynamically
                              ),
            dateRangeInput(
              inputId = "date_range",
              label = "Select Date Range:",
              #start = 2021-12-05,  # or set to min(prices$date)
              #end = Sys.Date(),         # or max(prices$date)
              start = NULL, # to be set dynamically
              end = NULL, # to be set dynamically
              format = "yyyy-mm-dd"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9,
            h3("Portfolio Summary"),
            textOutput("portLatestNote"),
            fluidRow(
                column(1,""),
                column(6, plotlyOutput("portfolioLatestPlot", height=ch_h, width=ch_w)),
                column(3, plotlyOutput("portROILatestPlot", height=ch_h, width=ch_w)),
                column(2,"")
            ),
            DTOutput("portfolioTotal"),
            h3("Portfolio Details by Coin"),
            plotlyOutput("portCoinLatestPlot", height=ch_h),
            DTOutput("portfolioLatest"),
            h3("Portfolio Trends"),
            plotlyOutput("portValueTrendPlot"),
            plotlyOutput("portValueTrendCoinPlot", height=ch_hx),
        )
    )
)
} # end opening request function