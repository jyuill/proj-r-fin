

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(quantmod)
library(shinythemes)
library(bslib)
library(dygraphs)
library(PerformanceAnalytics)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Stock Price Comparisons"),

    # Sidebar
    sidebarLayout(
      ## sidebar panel
        sidebarPanel(
            textInput(inputId='txtSym', label="Enter Symbol (sep with single space, no commas)", value="^GSPC META AMZN AAPL NFLX GOOG"),
            tags$a(href="https://finance.yahoo.com/lookup/", "Stock symbol lookup"),
            dateRangeInput(inputId='dtRng', label='Date Range', start='2022-01-01', end='2022-12-31' )
        ),
        ## main panel
        mainPanel(
            dygraphOutput("priceChart"),
            plotOutput("pa_corr")
        )
    )
)