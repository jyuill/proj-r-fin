## Fetch trade data from Google sheets

library(tidyverse)
library(lubridate)
library(here)
library(googledrive)
library(googlesheets4)

options(scipen=99)

## Use google sheet URL to identify file
sheet_id <- "https://docs.google.com/spreadsheets/d/1DRW5n6s6UtDAq3-kg-JTD4Yd2_24QfjAAxSU7-itkEE/edit?usp=sharing"

keyfile <- 'original-return-107905-3361ea00b21c.json'
gs4_auth(path=keyfile,
         scopes = "https://www.googleapis.com/auth/spreadsheets")

## use read_sheet to call file
sheet_data <- read_sheet(sheet_id, sheet='Transactions - Form') ## skips first row, which has meta info in this case
sheet_data <- sheet_data %>% filter(!is.na(Date))
sheet_data$Date <- date(sheet_data$Date)
## SAVE
write_csv(sheet_data, 'data/trades.csv')
write_csv(sheet_data, paste0('data/trades_',max(sheet_data$Date),'.csv'))

## calculate costs
trade_data <- sheet_data %>% arrange(Symbol, Date)
trade_data <- trade_data %>% group_by(Symbol) %>% mutate(
  units_chg=ifelse(Action=='Buy', Units, Units*-1),
  units_cum=cumsum(units_chg),
  ## remove units from sale based on units * prev acb (but error)
  spend=ifelse(Action=='Buy', Total, 0),
  #spend=ifelse(Action=='Buy', Total, 0-(Units*lag(acb))),
  spend_cum=cumsum(spend),
  #spend_net=ifelse(Action=='Buy', cumsum(spend), lag(spend_net)-(Units*lag(acb))), ## remove value of assets sold
  #spend_net=ifelse(Action=='Buy', lag(spend_net)+Total, lag(spend_net-(Units*lag(acb)))),
  #spend_net=cumsum(spend),
  acb_initial=ifelse(units_cum>0.0001, spend_cum/units_cum,0),
  spend_net=ifelse(Action=='Buy', Total, 0-(Units*lag(acb_initial))),
  spend_net_cum=cumsum(spend_net),
  acb=ifelse(units_cum>0.0001, spend_net_cum/units_cum,0)
)