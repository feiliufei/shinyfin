# load libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(PortfolioAnalytics)
library(highcharter)

# get data from gihub 
github <-"https://github.com/feiliufei/shinyfin/blob/master/"

prices_daily_all <- 
  as.xts(read.csv.zoo(paste0(github, 'djia_prices_all.csv'), 
                      format = "%Y-%m-%d", sep = ""))
symbols_all <- 
  read.csv(paste0(github, 'djia_comp.csv'), 
           sep = ';', stringsAsFactors = F) 

%>%
  .[-c(2, 22, 27), ]
