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

# Define server logic required to draw a histogram
function(input, output, session) {
  ## get price data based on inputs for use elsewhere
  symData_all <- reactive({
    syms <- input$txtSym
    sym_list <- str_split(syms, ",")
    sym_list <- sym_list[[1]]
    symData_all <- NULL
    for(symbs in sym_list){
      cat(symbs)
      symData_all <- cbind(symData_all,
                           getSymbols(Symbols=symbs,
                                      from=input$dtRng[1], to=input$dtRng[2], auto.assign=FALSE, src='yahoo'))
    }
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
