

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
    titlePanel("Stock Price Charts"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      ## sidebar panel
        sidebarPanel(
            textInput(inputId='txtSym', label="Enter Symbol (comma sep, no space)", value="^GSPC"),
            dateRangeInput(inputId='dtRng', label='Date Range', start='2022-01-01', end='2022-12-31' )
        ),
        ## main panel
        mainPanel(
            dygraphOutput("priceChart"),
            plotOutput("pa_corr")
        )
    )
)