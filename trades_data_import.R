## Fetch trade data from Google sheets
## Calculate average cost

library(tidyverse)
library(lubridate)
library(here)
library(googledrive)
library(googlesheets4)

options(scipen=99)

## STEPS: DOWNLOAD DATA ####
## 1. CB > John > eDocs > activity report > download csv
## 2. Go to google sheet (URL below)
## 3. Check most recent transaction on sheet
## 4. Copy/Paste new transactions from downloaded csv to sheet below existing

## google auth - see 'google-cred-info.R' ####
keyfile <- 'original-return-107905-3361ea00b21c.json' ## ?
keyfile <- 'original-return-107905-8542c2ab0ad1.json' ## mac
gs4_auth(path=here(keyfile),
         scopes = "https://www.googleapis.com/auth/spreadsheets")

## Get Google sheet ####
## Use google sheet URL to identify file
sheet_id <- "https://docs.google.com/spreadsheets/d/1DRW5n6s6UtDAq3-kg-JTD4Yd2_24QfjAAxSU7-itkEE/edit?usp=sharing"

## use read_sheet to call file
## initial manual sheet - not used anymore but code maintained until system switched over 
sheet_data <- read_sheet(sheet_id, sheet='Transactions - Form') ## skips first row, which has meta info in this case
sheet_data <- sheet_data %>% filter(!is.na(Date))
sheet_data$Date <- date(sheet_data$Date)

## > CB EXPORT SHEET ####
sheet_data_exp <- read_sheet(sheet_id, sheet='krypton-exp', skip=1) 
sde <- sheet_data_exp
sheet_data_exp <- sheet_data_exp %>% rename(
  Date_time=Date
) %>% mutate(
  Date=date(Date_time)
) %>% select(`Receipt No`, Date_time, Date, c(3:ncol(sheet_data_exp)))

## SAVE RAW DATA ####
## original manual sheet
write_csv(sheet_data, 'data/trades_data.csv')
write_csv(sheet_data, paste0('data/trades_data',max(sheet_data$Date),'.csv'))
## > CB exp sheet ####
write_csv(sheet_data_exp, 'data/trades_data_exp.csv')
write_csv(sheet_data_exp, paste0('data/trades_data_exp_',max(sheet_data_exp$Date),'.csv'))

## xCALC. COSTS [deprecated] ####
## cost calculations made in new CB exp sheet in google sheets
trade_data <- sheet_data %>% arrange(Symbol, Date)
trade_data <- trade_data %>% group_by(Symbol) %>% mutate(
  units_chg=ifelse(Action=='Buy', Units, Units*-1),
  units_cum=cumsum(units_chg),
  ## remove units from sale based on units * prev acb (but error) - so need extra cols as work-around
  #spend=ifelse(Action=='Buy', Total, 0-(Units*lag(acb))),
  spend=ifelse(Action=='Buy', Total, 0),
  spend_cum=cumsum(spend),
  ## initial acb calc based on total spend without removing units lost in sales
  acb_initial=ifelse(units_cum>0.0001, spend_cum/units_cum,0),
  ## remove spending on units sold based on acb_initial
  spend_net=ifelse(Action=='Buy', Total, 0-(Units*lag(acb_initial))),
  spend_net_cum=cumsum(spend_net),
  ## actual acb based on spend/units with sales removed
  ## - works but doesn't provide acb reference for sell transactions, to determine return
  ## - trying to get prev existing acb for reference on sell transactions -> issues with lag
  acb=ifelse(Action=='Buy', spend_net_cum/units_cum, lag(spend_net_cum)/lag(units_cum)),
  #acb=ifelse(units_cum>0.0001, spend_net_cum/units_cum,0),
  unit_gain=ifelse(Action=='Sell', Price-acb,NA),
  total_gain=ifelse(Action=='Sell', Total-(Units*acb),NA)
)

## xSAVE [deprecated] ####
write_csv(trade_data, 'data/trades_cost_data.csv')
write_csv(trade_data, paste0('data/trades_cost_data',Sys.Date(),'.csv'))