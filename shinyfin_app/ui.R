source('utilities.R')

##### Header ####
header <- dashboardHeader(title = "Shiny Finance")

##### Sidebar ####
sidebar <- dashboardSidebar(

  # ---- sidebar menus ----
  sidebarMenu(id = 'menu',
              menuItem(text = 'Stock Pick', tabName = 'stock_list'),
              menuItem(text = 'Returns of Selected Stocks', tabName = 'returns'),
              menuItem(text = 'Risk of 1/N Portfolio', tabName = 'risks'),
              menuItem(text = 'Portfolio Theory', tabName = 'capm'))
  )

#### Body ####
body <- dashboardBody(
  
  tabItems(
    
    # ---- stock list ----
    tabItem(tabName = 'stock_list',
            
            column(width = 12,
                   tags$p(strong('Monthly returns of 27 companies in the Dow Johns Industry Average Index are store in the dataset.')),
                   tags$p(strong('Please choose stocks\' symbols from the following table in the selectize, then click \"Submit\".')),
            
                   # selectize for stocks
           
                   selectizeInput(inputId = 'stocks', 
                                  label = "",
                                  choices = colnames(prices_daily_all)[order(colnames(prices_daily_all))],
                                  multiple = T,
                                  width = '50%'),
                   # submit button
                   actionButton('submit', 'Submit')),
           
            tags$br(), tags$br(),
            
            column(dataTableOutput('id_stock'), width = 12)
            ),
    
    # --- returns ----
    tabItem(tabName = 'returns',
            
            fluidRow(valueBoxOutput('n_stock'),
                     valueBoxOutput('ret_annu'),
                     valueBoxOutput('ret_value')),
            
            fluidRow(
              column(width = 4,
                selectInput('rebal', 
                            'Portfolio Rebalancing Frequancy', 
                             choices = c('Yearly' = 'years', 
                                         'Monthly' = 'months'), width = '100%'),
                         
                actionButton('next1', 'Next Section')),
              
              column(width = 8, 
                     box(highchartOutput('plot_return'), width = 12, collapsible = T))
            )
            ),
    
    # --- risks ----
    tabItem(tabName = 'risks',
            
            fluidRow(
              valueBoxOutput('sd_eq'),
              valueBoxOutput('skewness_eq'),
              valueBoxOutput('kurtosis_eq')
              ),
            
            fluidRow(
              box(dataTableOutput('sd_table'), width = 4),
              box(highchartOutput('ret_risk'), width = 4),
              box(highchartOutput('dis_eq'), width = 4)
              ),
            
            hr(),
            
            fluidRow(
              column(width = 4,
                     selectInput('windows', 
                              'Rolling Windows Frequancy', 
                              choices = c('6 Months' = 6, '12 Months' = 12, '24 Months' = 24),
                              width = '100%')
              )
              ),
            
            fluidRow(
              box(highchartOutput('roll_sd_eq'), width = 4),
              box(highchartOutput('roll_skewness_eq'), width = 4),
              box(highchartOutput('roll_kurtosis_eq'), width = 4)
              ),
            
            fluidRow(
              column(width = 12, actionButton('next2', 'Next Section'))
              )
            ),
    
    # ---- portfolio theory ----
    tabItem(tabName = 'camp')
  )
)


#### shinyUI ####
shinyUI(
  dashboardPage(skin = 'green',
                header = header,
                sidebar = sidebar,
                body = body)
  )