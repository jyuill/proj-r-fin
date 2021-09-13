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
library(DT)
library(RColorBrewer)
library(gridExtra)
library(here)
library(plotly)

options(scipen = 10)
dollar_format(accuracy=1000)

theme_set(theme_bw())

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    sim_all <- eventReactive(input$runsim, {
        #data.frame(start=runif(n=input$nsims, min=input$sbal[1], max=input$sbal[2]))
         sim_all <- data.frame()
         for(s in 1:input$nsims){
             sim <- s
             bal_start <- round(runif(n=1, min=input$sbal[1], max=input$sbal[2]),0)
             bal_start_set <- bal_start
             bal_start_no_draw <- bal_start
             sim_sched <- data.frame()
             #sim <- data.frame(sim=s, start=bal_start)
             #sim_all <- bind_rows(sim_all, sim)
             for(y in 1:input$nyrs){
                 rrate <- round(min(rnorm(n=1, mean=input$arr, sd=input$sdrr), input$maxrr), 3)
                 return <- ifelse(bal_start>0,round(bal_start*rrate, 0),0)
                 draw_ni <- min(round(runif(n=1, min=input$draw[1], max=input$draw[2]),0),bal_start+return)
                 draw <- min(round(draw_ni*(1+input$inflr)^y,0),bal_start+return)
                 bal_remain <- bal_start+return-draw
                 bal_no_draw <- round(bal_start_no_draw*(1+rrate),0)
                 sim_yr <- data.frame('sim'=sim,
                                      'year'=y,
                                      'start'=bal_start,
                                      'rrate'=rrate,
                                      'return'=return,
                                      'draw_ni'=draw_ni,
                                      'draw'=draw,
                                      'balance'=bal_remain,
                                      'bal_no_draw'=bal_no_draw)
                 sim_sched <- bind_rows(sim_sched, sim_yr)
                 bal_start <- bal_remain
                 bal_start_no_draw <- bal_no_draw
             }
             sim_all<- bind_rows(sim_all, sim_sched)
         }
         sim_all$sim <- as.factor(sim_all$sim)
         sim_all_s <<- sim_all ## save to global environment
         return(sim_all)
    })
    
    sim_check <- eventReactive(input$runsim, {
        ## STATUS
        sim_all <- sim_all_s %>% mutate(
            status=ifelse(balance>0, "money","no_money")
        )
        sim_20 <- sim_all %>% filter(year==20) 
        sim_20$status <- factor(sim_20$status, levels=c('no_money','money'))
        
        ## Get Status for selected years
        yr_st <- 10
        sim_check_all <- data.frame()
        for(y in yr_st:20){
            yr_check <- y
            sim_check <- sim_all %>% filter(year==yr_check) %>% group_by(status) %>% summarize(status_count=n()) %>% mutate(yr=yr_check)
            sim_check_all <- bind_rows(sim_check_all, sim_check)
        }
        ## set factor levels for more control
        sim_check_all$status <- factor(sim_check_all$status, levels=c('no_money','money')) 
        ## bar chart for split between money / no-money
        # p1 <- sim_check_all %>% ggplot(aes(x=as.factor(yr), y=status_count, fill=status))+geom_col(position='fill')+
        #   scale_y_continuous(labels=percent, expand=expansion(add=c(0,0)))+
        #   scale_fill_manual(values=s_color)+
        #   labs(x='years from start', y='')
        
        ## spread data to calc percentages of cases with money
        sim_check_all_wide <- sim_check_all %>% pivot_wider(names_from=status, values_from=status_count) %>% mutate(
            status_pc=money/(money+no_money)
        ) 
        return(sim_check_all_wide)
    })
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        sim_all() %>% ggplot(aes(x=start))+geom_histogram()

    })
    output$burndownPlot <- renderPlotly({
        psim <- sim_all() %>% ggplot(aes(x=year, y=balance, color=sim))+geom_line()+
            geom_hline(yintercept=0)+
            scale_x_continuous(expand=c(0,0))+
            scale_y_continuous(labels=dollar, expand=expansion(mult=c(0,0.01)))+
            theme(legend.position = 'none')+
            labs(y="", x="year from start")
        #sim_all() %>% ggplot(aes(x=year, y=balance, color=sim))+geom_line()
        
        ggplotly(psim)
    })
    output$probPlot <- renderPlotly({
        ## line chart showing percent with money each yr
        p2 <- sim_check() %>% ggplot(aes(x=as.factor(yr), y=status_pc))+geom_line(group=1)+
            scale_y_continuous(labels=percent, expand=c(0,0), limit=c(0,1))+
            scale_x_discrete(expand=c(0,0))+
            labs(x='years from start', y='% of simulations')
        
        ggplotly(p2)
    })
    

})
