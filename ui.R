source('utilities.R')

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
              menuItem(text = 'Returns', tabName = 'returns'),
              menuItem(text = 'Risks', tabName = 'risks'))
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
    tabItem(tabName = 'returns',
            fluidRow(valueBoxOutput('n_stock'),
                     valueBoxOutput('ret_annu'),
                     valueBoxOutput('ret_value')),
                     br(),br(),
                     
            fluidRow(box(highchartOutput('plot_return'), width = 12))),
    
    # content of returns
    tabItem(tabName = 'risks')
  )
)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = 'green',
                      header = header,
                      sidebar = sidebar,
                      body = body))