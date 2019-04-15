# load libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(PortfolioAnalytics)
library(highcharter)
library(readr)

# get data from gihub
github <-"https://raw.githubusercontent.com/feiliufei/shinyfin/master/"

prices_daily_all <-
  as.xts(read.csv.zoo(paste0(github, 'djia_prices_all.csv'),
                      format = "%Y-%m-%d", sep = ""))
symbols_all <-
  read.csv('djia_comp.csv', sep = ',', stringsAsFactors = F) %>% .[-c(2, 22, 27), ]

xts_to_df <- function(xts) {
  data.frame(xts, date = index(xts)) %>% 
    remove_rownames() %>% 
    select(date, colnames(xts))
}