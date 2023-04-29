#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(quantmod)
library(shinythemes)
library(bslib)
library(dygraphs)
library(PerformanceAnalytics)
library(here)

## TEST AREA - manual (can leave as-is won't affect app)
sym_list <- c("GOOG","MSFT")
dtRng <- c('2022-01-01','2023-03-31')
symData_all <- NULL
## loop through to get data for each symbol
for(symbs in sym_list){
  cat(paste0("syms: ", syms, " symbs: ",symbs, "\n"))
  symData_all <- cbind(symData_all,
                       getSymbols(Symbols=symbs,
                                  from=dtRng[1], to=dtRng[2], 
                                  auto.assign=FALSE, src='yahoo'))
}
## combine results of each loop 
symData_all

# Define server logic required to draw a histogram
function(input, output, session) {
  ## get price data based on inputs for use elsewhere
  symData_all <- reactive({
    syms <- input$txtSym ## space-separated items
    sym_list <- str_split_1(syms, " ") ## splits the space-sep items into list
    ## empty data frame to hold results of loop
    symData_all <- NULL
    ## loop through to get data for each symbol
    for(symbs in sym_list){
      cat(paste0("syms: ", syms, " symbs: ",symbs, "\n"))
      symData_all <- cbind(symData_all,
                           getSymbols(Symbols=symbs,
                                      from=input$dtRng[1], to=input$dtRng[2], auto.assign=FALSE, src='yahoo'))
    }
    ## combine results of each loop 
    symData_all
  })
  ## price chart - show data collected above to compare symbols
    output$priceChart <- renderDygraph({
      symData <- symData_all()
      dygraph(Cl(symData)) %>% dyRangeSelector()
      
    })
    
    output$pa_corr <- renderPlot({
      symData <- symData_all()
     # charts.PerformanceSummary(Cl(symData), main="Perf Summ",
      #                          geometric = FALSE, wealth.index=TRUE)
      chart.Correlation(Cl(symData))
    })
}
