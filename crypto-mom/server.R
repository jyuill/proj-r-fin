#
# CRYPTO-MOM: This is the server logic of a Shiny web application.

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
library(openssl) # possibly needed for decoding base64 encoded json file

# basic settings
theme_set(theme_bw())
bar_colr <- "#2c3e50" # dark blue
#bar_colr <- "blue"

# load functions ----
# running locally
source(here("crypto-mom/functions.R"))
#source("functions.R")

# Get data ----
## portfolio info ----
## set up google auth
# old way where Sys.getenv environment variable is just path to the json file
# if(Sys.getenv("GOOGLE_AUTH_JSON")!=""){
#   gs4_auth(path = Sys.getenv("GOOGLE_AUTH_JSON"))
# } else {
#   stop("Please set up google auth json file")
# }
# new way for Posit Connect cloud
json_base64 <- Sys.getenv("GOOGLE_AUTH_JSON")
if (nzchar(json_base64)) {
  json_raw <- base64_decode(json_base64)
  temp_json <- tempfile(fileext = ".json")
  writeBin(json_raw, temp_json)
  gs4_auth(path = temp_json)
} else {
  stop("Missing GOOGLE_AUTH_JSON environment variable")
}


gsheet <- 'https://docs.google.com/spreadsheets/d/1DRW5n6s6UtDAq3-kg-JTD4Yd2_24QfjAAxSU7-itkEE/edit?usp=sharing'
sheet_item <- 'crypto-mom'

portfolio <- read_sheet(ss=gsheet, sheet=sheet_item, skip=2)

## get prices ----
## get latest prices - cycle thru each coin in portfolio list
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

### merge with portfolio ----
### daily historical ----
## combine prices with portfolio info for each day
prices_long <- prices %>%
  pivot_longer(cols = -date, names_to = "coin", values_to = "price") %>%
  mutate(date = as.Date(date))
### MOTHER: prices_long_port ----
# everything flows from this in reactives
prices_long_port <- full_join(portfolio, prices_long, by = "coin") %>%
  mutate(date = as.Date(date)) %>%
  select(coin, date_purch, amt_purch, price_purch, total_invest, date, price) %>%
  mutate(total_value = price * amt_purch,
         gain = total_value - total_invest,
         roi = gain / total_invest)
prices_long_port$date_purch <- as.Date(prices_long_port$date_purch)

# summary data for testing - all coins
port_ttl_date <- prices_long_port %>%
  group_by(date) %>%
  summarize(
    total_invest = sum(total_invest, na.rm = TRUE),
    total_value = sum(total_value, na.rm = TRUE),
    total_gain = sum(gain, na.rm = TRUE),
    total_roi = total_gain / total_invest
  ) %>%
  mutate(date = as.Date(date))

# Define server logic ----
function(input, output, session) {
    # data
  # filter setup ####
  # coin selections
  observe({
    req(portfolio)
    coin_choices <- unique(portfolio$coin)
    updateCheckboxGroupInput(session, 
                        inputId = "selected_coins",
             choices = coin_choices,
             selected = coin_choices)
  })
  # date range - based on dates available in data
  observe({
    req(prices)
    # update date range input with min and max dates from portfolio   
    updateDateRangeInput(
      session,
      inputId = "date_range",
      start = min(prices$date),
      end = max(prices$date)
    )
  })
  
  # end filter setup ----
  # reactives for filtering ----
  ## MAIN: prices_long_port_filtered is combined source of all data ----
  prices_long_port_filtered <- reactive({
        req(input$selected_coins)
        req(input$date_range)
        prices_long_port %>% filter(coin %in% input$selected_coins,
                                    date >= input$date_range[1],
                                    date <= input$date_range[2])
    })
  
  ## various reactives - derived from above for particular purposes
  ## prep: port_ttl_dates ----
  port_ttl_dates <- reactive({
        req(prices_long_port_filtered())
        prices_long_port_filtered() %>%
            # groups all coins by date for total
            group_by(date) %>%
            summarize(
                total_invest = sum(total_invest, na.rm = TRUE),
                total_value = sum(total_value, na.rm = TRUE),
                total_gain = sum(gain, na.rm = TRUE),
                total_roi = total_gain / total_invest
            ) %>%
            mutate(date = as.Date(date))
    })
  ## prep: port_price_latest_filtered ----
  port_price_latest_filtered <- reactive({
        req(prices_long_port_filtered())
        prices_long_port_filtered() %>%
            filter(date == max(date)) 
    })
  ## prep: port_price_latest_fltr_lng ----
  port_price_latest_fltr_lng <- reactive({
  port_price_latest_filtered() %>%
    ### TEST ----
    #port_price_latest_lng_test <- port_price_latest_test %>%
    select(date, coin, total_invest, total_value, gain, roi) %>%
    pivot_longer(cols = c(total_invest, total_value, gain, roi),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(
      metric = factor(metric, 
                      levels = c("total_invest", "total_value", "gain", "roi"),
                      labels = c("Invested", "Value", "Gain", "ROI"))
    ) %>% mutate(
      condition_clr = ifelse(metric %in% c("Gain","ROI") & value>0, "green",
                             ifelse(metric %in% c("Gain","ROI") & value<0, "red", bar_colr))
    )
  })
  
  ## prep: portfolio_ttl_return_filtered ----
  portfolio_ttl_return_filtered <- reactive({
        req(port_price_latest_filtered())
        port_price_latest_filtered() %>%
          # groups all coins by date for total
            group_by(date) %>%
            summarize(
                total_invest = sum(total_invest, na.rm = TRUE),
                total_value = sum(total_value, na.rm = TRUE),
                total_gain = sum(gain, na.rm = TRUE),
                total_roi = total_gain / total_invest
            ) %>%
            mutate(date = as.Date(date))
    })
    ## prep: port_long_filtered ----
    port_long_filtered <- reactive({
        req(portfolio_ttl_return_filtered())
        portfolio_ttl_return_filtered() %>% 
            mutate(date = as.Date(date)) %>%
            pivot_longer(cols = -date,
                         names_to = "metric",
                         values_to = "value") %>%
            mutate(
                metric = factor(metric, 
                                levels = c("total_invest", "total_value", "total_gain", "total_roi"),
                                labels = c("Invested", "Value", "Total Gain", "ROI"))
            ) %>% mutate(
              condition_clr = ifelse(metric %in% c("Total Gain","ROI") & value>0, "green",
                                     ifelse(metric %in% c("Total Gain","ROI") & value<0, "red", bar_colr))
            )
    })
    
  # PLOTS ----
    # plot: overall port value ----
    output$portfolioLatestPlot <- renderPlotly({
        # plot latest overall portfolio value
        p <- port_long_filtered() %>% filter(metric!="ROI") %>%
            ggplot(aes(x = metric, y = value, fill=condition_clr)) +
            geom_col() +
            scale_fill_identity() + # for conditional colors
            scale_y_continuous(labels = dollar, expand = expansion(add = c(0, 100))) +
            labs(x = "", y = "$") +
            theme(legend.position = "none",
                  axis.ticks.x = element_blank())
        ggplotly(p)
    })
    # plot: overall ROI ----
    output$portROILatestPlot <- renderPlotly({
      # plot latest overall portfolio value
      p <- port_long_filtered() %>% filter(metric=="ROI") %>%
        ggplot(aes(x = metric, y = value, fill=condition_clr)) +
        geom_col() +
        scale_fill_identity() + # for conditional colors
        geom_text(aes(label= scales::percent(value, accuracy = 0.1)), 
                  position = position_stack(vjust = 4), 
                  size = 6, color = "black") +
        scale_y_continuous(labels = label_percent(), expand = expansion(add = c(0, 1))) +
        labs(x = "", y = "% Return") +
        theme(legend.position = "none",
              axis.ticks.x = element_blank())
      ggplotly(p)
    })
  
    # plot: individual coin returns ----
    output$portCoinLatestPlot <- renderPlotly({
        # plot latest individual coin returns
        p <- port_price_latest_fltr_lng() %>%
            ## TESTING ----
            #port_price_latest_lng_test %>%
            filter(metric != "ROI") %>%
            ggplot(aes(x = metric, y = value, fill=condition_clr)) +
            geom_col() +
            scale_fill_identity() + # for conditional colors
            facet_grid(.~coin) +
            geom_hline(yintercept = 0, linetype = "solid", color = "black") +
            scale_y_continuous(labels = dollar, expand = expansion(add = c(0, 100))) +
            labs(x = "", y = "$") +
            theme(legend.position = "none",
                  axis.ticks.x = element_blank())
        ggplotly(p)
    })
  
    # plot: total value trend ----
    output$portValueTrendPlot <- renderPlotly({
        # plot total value trend
        p <- port_ttl_dates() %>% 
            ## testing ----
            #port_ttl_date %>%
            ggplot(aes(x = date, y = total_value)) +
            geom_line(size = 1, color=bar_colr) +
            geom_line(aes(y=total_invest), color="lightgreen", size=1.2)+
            scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.01))) +
            labs(x = "", y = "$", title = "Total Portfolio Value Trend") +
            theme(legend.position = "bottom")
        ggplotly(p)
    })
    # plot: ttl val trend by coin ----
    output$portValueTrendCoinPlot <- renderPlotly({
        # plot total value trend by coin
        p <- prices_long_port_filtered() %>%
            # TESTING ----
            #prices_long_port %>%
            ggplot(aes(x = date, y = total_value)) +
            geom_line(size = 1, color=bar_colr) +
            geom_line(aes(y = total_invest), color = "lightgreen", size=1.2) +
            facet_grid(coin~ ., scales="free_y") + 
            scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.01))) +
            labs(x = "", y = "$", title = "Total Value Trend by Coin") +
            theme(legend.position = "bottom")
        ggplotly(p)
    })
    # TBL: portfolio total ----
    output$portfolioTotal <- renderDataTable({
        datatable(portfolio_ttl_return_filtered(), 
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
    # TBL: coin returns ----
    output$portfolioLatest <- renderDataTable({
        datatable(port_price_latest_filtered()[, c("coin", 
                                       "date_purch", "amt_purch", "price_purch", 
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
