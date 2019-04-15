library(shinydashboard)
library(shiny)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(PortfolioAnalytics)
library(highcharter)

# prices_daily_all <- as.xts(read.csv.zoo('djia_prices_all.csv', format = "%Y-%m-%d", sep = ""))
# symbols_all <- read.csv('djia_comp.csv', sep = ';', stringsAsFactors = F)
# symbols_all <- symbols_all[-c(2, 22, 27), ]

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
  

# Server
server <- function(input, output) {
  
  # table of company names
  output$id_stock <- renderDataTable(symbols_all, 
                                     options = list(
                                       pageLength = 5, 
                                       lengthMenu = c(5, 10, 15, 20)))
  
  observeEvent(input$submit, {val_re$stocks_selected <- input$stocks})
  val_re <- reactiveValues(stocks_selected = head(symbols_all$Symbol))
  
  prices_daily <- reactive(prices_daily_all[, val_re$stocks_selected])
  prices <- reactive(to.monthly(prices_daily(), indexAt = "firstof", OHLC = F))
  prices_long <- reactive(prices() %>% data.frame(date = index(.)) %>% gather(asset, prices, -date))
  
  # widget of count of stocks selected
  output$n_stock <- renderValueBox({valueBox(value = ncol(prices_daily()),
                                             subtitle = "stocks selected",
                                             icon = icon("lightbulb-o"))})
  
  # individuall price trend and 1/n portfolio price trend
  output$plot_price <- renderHighchart({hchart(prices_long(), 'line', 
                                               hcaes(x = date, y = prices, group = asset))})

  # 
  # output$plot_price <- renderPlot({chart.TimeSeries(prices_daily(), 
  #                                                   main = 'Stock Price 1990-2018',
  #                                                   legend.loc = 'topleft', 
  #                                                   xlab = 'Time', 
  #                                                   ylab = "Price in USD",
  #                                                   date.format = '%y-%m')})

}

# prices_daily <- prices_daily_all[, head(symbols_all$Symbol)]
# UI
ui <- dashboardPage(skin = 'green',
                    header = header,
                    sidebar = sidebar,
                    body = body)

shinyApp(ui, server)