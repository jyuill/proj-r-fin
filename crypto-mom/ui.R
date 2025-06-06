#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(shinythemes)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Crypto Portfolio Status"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
        mainPanel(
            h3("Portfolio Summary"),
            fluidRow(
                column(7, plotlyOutput("portfolioLatestPlot")),
                column(3, plotlyOutput("portROILatestPlot")),
                column(2,"")
            ),
            #plotlyOutput("portfolioLatestPlot"),
            #plotlyOutput("portROILatestPlot"),
            DTOutput("portfolioTotal"),
            h3("Portfolio Details by Coin"),
            DTOutput("portfolioLatest"),
        )
    )
)
