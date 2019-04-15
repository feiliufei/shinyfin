#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shinydashboard)
library(shiny)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(PortfolioAnalytics)
library(highcharter)



# Header
header <- dashboardHeader()

# Sidebar
sidebar <- dashboardSidebar(
  
  # selectize for stocks
  selectizeInput(inputId = 'stocks',
                 label = 'Input Stock ID to select stocks',
                 choices = colnames(prices_daily_all),
                 multiple = T),
  
  # submit button
  actionButton('submit', 'Submit'),
  
  # sidebar menus
  sidebarMenu(menuItem(text = 'Stock List', tabName = 'stock_list'),
              menuItem(text = 'Stocks', tabName = 'stocks'),
              menuItem(text = 'Returns', tabName = 'returns'))
)

# Body
body <- dashboardBody(
  
  tabItems(
    
    # content of stock list
    tabItem(tabName = 'stock_list',
            
            HTML('Choose stocks\' symbols from the follwing table in the left selectize, then click Submit.'),
            br(),br(),
            
            fluidRow(
              column(width = 12,
                     #div(style = "height:200px"),
                     dataTableOutput('id_stock')))),
    
    # content of stocks
    tabItem(tabName = 'stocks',
            fluidRow(
              valueBoxOutput('n_stock'),
              highchartOutput('plot_price'))),
    
    # content of returns
    tabItem(tabName = 'returns')
  )
)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = 'green',
                      header = header,
                      sidebar = sidebar,
                      body = body))


