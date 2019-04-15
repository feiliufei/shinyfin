shinyServer(function(input, output) {
  
  # table of company names
  output$id_stock <- 
    renderDataTable(symbols_all, 
                    options = list(
                      pageLength = 10, 
                      lengthMenu = c(10, 20, 30)))
  
  observeEvent(input$submit, {val_re$stocks_selected <- input$stocks})
  
  val_re <- 
    reactiveValues(stocks_selected = head(symbols_all$Symbol))
  
  prices <- 
    reactive(to.monthly(prices_daily_all[, val_re$stocks_selected], indexAt = "firstof", OHLC = F))
  
  returns <-
    reactive(Return.calculate(prices(), method = 'log') %>% na.omit)
  
  # prices_long <- 
  #   reactive(prices() %>% data.frame(date = index(.)) %>% gather(asset, prices, -date))
  # 
  # returns_long  <- 
  #   reactive(prices_long() %>% 
  #              group_by(asset) %>%
  #              mutate(returns = log(prices) - log(lag(prices))) %>%
  #              select(-prices) %>%
  #              na.omit())
  
  
  
  # weight of equally weighted portfolio
  w_eq <- 
    reactive(rep(1/length(val_re$stocks_selected), length(val_re$stocks_selected)))
  
  ret_eq <- 
    reactive(Return.portfolio(returns(), weights = w_eq(), rebalance_on = 'years') %>% 'colnames<-'('Equal_W'))
  
  ret_annu <- 
    reactive(Return.annualized.excess(cbind(returns(), ret_eq()), Rb = 0, scale = 12, geometric = T))
  
  # widget of count of stocks selected
  output$n_stock <- 
    renderValueBox({valueBox(value = ncol(prices()), 
                             subtitle = "stocks selected",
                            icon = icon('fas fa-chart-line'),
                            color = 'green')})
  
  # widget of annulized return of 1/n portfolio
  output$ret_annu <- 
    renderValueBox({valueBox(value = paste(round(ret_annu(), 4) * 100, "%"),
                             subtitle = 'annulized return of equal-weighted portfolio',
                             icon = icon('fas fa-percentage'),
                             color = 'green')})
  
  # widget of annulized return of 1/n portfolio
  output$ret_value <- 
    renderValueBox({valueBox(value = paste(round((1+ Return.cumulative(ret_eq(), geometric = T))*100, 2), 'Euro'),
                             subtitle = 'value if 100 Euro was invested in the portfolio',
                             icon = icon('fas fa-money-bill-alt'),
                             color = 'green')})
  
  
  # individuall price trend and 1/n portfolio price trend
  output$plot_return <- 
    renderHighchart({
      round(cumsum(cbind(returns(), ret_eq())), 2) %>%
        xts_to_df() %>%
        gather(symbols, returns, -date) %>%
        hchart(type = 'line', hcaes(x = date, y = returns, group = symbols))})
  
  # 
  # output$plot_price <- renderPlot({chart.TimeSeries(prices_daily(), 
  #                                                   main = 'Stock Price 1990-2018',
  #                                                   legend.loc = 'topleft', 
  #                                                   xlab = 'Time', 
  #                                                   ylab = "Price in USD",
  #                                                   date.format = '%y-%m')})
  
}
)
