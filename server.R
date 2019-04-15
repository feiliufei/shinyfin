


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
)
