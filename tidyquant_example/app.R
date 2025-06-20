# From ChatGPT: Tidyquant example

library(shiny)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(DT)

crypto_choices <- c("BTC-USD", "ETH-USD", "SOL-USD", "ADA-USD")

ui <- fluidPage(
  titlePanel("Crypto Price Tracker (Normalized with Moving Averages)"),
  
  sidebarLayout(
    sidebarPanel(
      #checkboxGroupInput("symbols", "Select Cryptos:", 
      #                   choices = crypto_choices, 
      #                   selected = c("BTC-USD", "ETH-USD")),
      #textInput("symbols", "Enter Crypto Symbols (comma-separated):", 
      #         value = "BTC-USD, ETH-USD"),
      fileInput("upload_csv", "Upload Portfolio CSV", 
                accept = c(".csv")),
      actionButton("save_btn", "Save upload"),
      dateRangeInput("dates", "Date Range:", 
                     start = Sys.Date() - 180, end = Sys.Date()),
      sliderInput("ma_window", "Moving Average Window (days):", min = 5, max = 50, value = 20)
    ),
    
    mainPanel(
      plotOutput("price_plot"),
      DTOutput("portfolio_table"),
    )
  )
)

server <- function(input, output, session) {
  # Default portfolio
  default_data <- data.frame(
    symbol = c("BTC-USD", "ETH-USD"),
    date = as.character(Sys.Date()),
    quantity = c(0.1, 0.5),
    price = c(40000, 2500),
    stringsAsFactors = FALSE
  )
  
  # Reactive value for portfolio
  portfolio <- reactiveVal(default_data)
  
  # Update portfolio if user uploads CSV
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    tryCatch({
      uploaded <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
      validate(
        need(all(c("symbol", "date", "quantity", "price") %in% colnames(uploaded)),
             "CSV must contain columns: symbol, date, quantity, price")
      )
      portfolio(uploaded)
      showNotification("Portfolio updated from CSV!", type = "message")
    }, error = function(e) {
      showNotification("Error reading CSV: Check format", type = "error")
    })
  })
  
  # Render editable table
  output$portfolio_table <- renderDT({
    datatable(portfolio(), editable = TRUE, rownames = FALSE)
  })
  
  # Apply edits from the DT table
  observeEvent(input$portfolio_table_cell_edit, {
    info <- input$portfolio_table_cell_edit
    updated <- portfolio()
    #updated[info$row, info$col] <- info$value
    col_name <- names(updated)[info$col+1]
    #updated[info$row, col_name] <- info$value
    updated[info$row, col_name] <- DT::coerceValue(info$value, updated[info$row, col_name])
    portfolio(updated)
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
    portfolio() %>%
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
