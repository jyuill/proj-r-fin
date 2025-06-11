# From ChatGPT: Tidyquant example

library(shiny)
library(tidyquant)
library(dplyr)
library(ggplot2)

crypto_choices <- c("BTC-USD", "ETH-USD", "SOL-USD", "ADA-USD")

ui <- fluidPage(
  titlePanel("Crypto Price Tracker (Normalized with Moving Averages)"),
  
  sidebarLayout(
    sidebarPanel(
      #checkboxGroupInput("symbols", "Select Cryptos:", 
      #                   choices = crypto_choices, 
      #                   selected = c("BTC-USD", "ETH-USD")),
      textInput("symbols", "Enter Crypto Symbols (comma-separated):", 
               value = "BTC-USD, ETH-USD"),
      dateRangeInput("dates", "Date Range:", 
                     start = Sys.Date() - 180, end = Sys.Date()),
      sliderInput("ma_window", "Moving Average Window (days):", min = 5, max = 50, value = 20)
    ),
    
    mainPanel(
      plotOutput("price_plot")
    )
  )
)

server <- function(input, output, session) {
  symbols <- reactive({
    strsplit(input$symbols, ",")[[1]] %>% 
      trimws() %>% 
      purrr::discard(~ .x =="")
  })
  crypto_data <- reactive({
    #req(input$symbols)
    req(length(symbols()) > 0)
    tryCatch({
      tq_get(
        symbols(),
        from = input$dates[1],
        to   = input$dates[2],
        get  = "stock.prices"
      )
    }, error = function(e) {
      showNotification(paste("One or more symbols invalid or unavaiable."), type = "error")
      NULL
    })
  })
  
  processed_data <- reactive({
    crypto_data() %>%
      group_by(symbol) %>%
      arrange(date) %>%
      mutate(
        price_norm = adjusted / first(adjusted),
        ma = TTR::SMA(price_norm, n = input$ma_window)
      ) %>%
      ungroup()
  })
  
  output$price_plot <- renderPlot({
    ggplot(processed_data(), aes(x = date, y = price_norm, color = symbol)) +
      geom_line(size = 1, alpha = 0.6) +
      geom_line(aes(y = ma, group = symbol), linetype = "dashed", size = 1) +
      labs(title = "Normalized Crypto Prices with Moving Average",
           x = NULL, y = "Relative Price (Start = 1)",
           caption = paste("SMA:", input$ma_window, "days")) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
