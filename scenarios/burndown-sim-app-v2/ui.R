#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(DT)
library(RColorBrewer)
library(gridExtra)
library(here)
library(plotly)

options(scipen = 10)
dollar_format(accuracy=1000)

theme_set(theme_bw())

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Retirement Funding Simulation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId='nsims', '# of Simulations', value=100, min=1, max=5000, step=50),
            numericInput(inputId='nyrs', "# of Yrs", value=20, min=5, max=30, step=1),
            sliderInput(inputId = 'sbal', label='Starting Bal. Range', min=650000, max=1500000,
                        step=50000, value=c(700000, 1000000), pre="$"),
            sliderInput(inputId='arr', label="Ave. Return",
                        value=0.03, min=0.02, max=0.06, step=0.005),
            sliderInput(inputId='sdrr', label="Std. Dev. Return",
                        value=0.07, min=0.05, max=0.1),
            sliderInput(inputId='maxrr', label="Max. Return",
                        value=0.2, min=0.15, max=0.25),
            sliderInput(inputId='draw', label="Annual Draw Range", 
                        value=c(55000,70000), min=50000, max=80000, step=5000, pre="$"),
            numericInput(inputId='inflr', label="Annual Inflation Rate",
                         value=0.02, min=0.01, max=0.1, step=0.005),
            actionButton(inputId='runsim', label="Run Sims")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            #plotOutput("distPlot"),
            plotlyOutput("burndownPlot"),
            plotlyOutput("probPlot")
        )
    )
))
