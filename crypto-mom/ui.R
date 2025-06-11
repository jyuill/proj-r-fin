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
library(bslib)

# settings
options(scipen = 10)
# chart size
ch_h <- '200px'
ch_hm <- '300px'
ch_hx <- '800px'
ch_w <- '90%'

# Define UI for application
# wrapping in function(request) at suggestion of chatGPT to control titlePanel when embedding in website
ui <- function(request) {
  # used to determine if app is embedded in website or not (when paired with url/?embed=true)
  query <- parseQueryString(request$QUERY_STRING)
  
fluidPage(
    # Application theme
    theme = bs_theme(version = 5, 
                     bootswatch = "sandstone"),
    tags$head(
      tags$link(href = "styles.css", rel= "stylesheet", type = "text/css")
      ),
    # Set the favicon)
    # Application title - doesn't show when embedded in website
    if (is.null(query$embed)) {
      titlePanel("Crypto Mom! Investment Portfolio Status")    
    },
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3, class = "sidebar", # sidebar panel ----
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
            ),
            h4("What is this?"),
            p(class="sider", "A basic dashboard for tracking status of a simple cryptocurrency portfolio. 
              For reference in considering future trades. "),
            p(class="sider", strong("'Price'"),"references closing price each day, or latest price for current day. 
              Updated when app is loaded."),
            h4("sources:"),
            tags$ul(class="sider",
              tags$li("Portfolio data imported from a Google Sheet"),
              tags$li("Price data: ",a(href="https://finance.yahoo.com/", "Yahoo! finance"),
              " via ", 
                      a(href = "https://www.quantmod.com/", "quantmod")),
              tags$li("App built with ", a(href = "https://shiny.rstudio.com/", "Shiny"))
            )
        ),

        # main panel ----
        mainPanel(width = 9, class=".main-panel", # main panel
            h3("Portfolio Summary"), ## summary ----
            textOutput("portLatestNote"),
            fluidRow(
                #column(1,""),
                column(6, 
                       #plotlyOutput("portfolioLatestPlot", height=ch_h, width=ch_w)
                       plotOutput("portfolioLatestPlot", height=ch_h, width=ch_w)
                       ),
                column(3, 
                       #plotlyOutput("portROILatestPlot", height=ch_h, width=ch_w)
                       plotOutput("portROILatestPlot", height=ch_h, width=ch_w)
                       ),
                column(3,
                       plotOutput("portValueCoinLatest", height=ch_h, width=ch_w))
            ),
            DTOutput("portfolioTotal"),
            h3("Portfolio Details by Coin"), ## coin details ----
            #plotlyOutput("portCoinLatestPlot", height=ch_h),
            plotOutput("portCoinLatestPlot", height=ch_h, width=ch_w),
            DTOutput("portfolioLatest"),
            h3("Portfolio Trends"), ## trends ----
            DTOutput("portfolioReturnsTable"),
            #plotlyOutput("portValueTrendPlot"),
            plotOutput("portValueTrendPlot", width=ch_w),
            #plotlyOutput("portValueTrendCoinPlot", height=ch_hx),
            plotOutput("portValueTrendCoinPlot", height=ch_hx, width=ch_w),
            h3("Relative Price Distributions"), ## relative price distributions ----
            plotOutput("portPriceDistCoinBoxPlot", height=ch_h, width=ch_w),
        ) # end main panel ----
    ) # end sidebarLayout ----
)
} # end opening request function