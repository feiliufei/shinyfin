# table of company names
output$id_stock <- 
  renderDataTable(symbols_all, 
                  options = list(pageLength = 10, lengthMenu = c(10, 20, 30)))

observeEvent(input$submit, {val_re$stocks_selected <- input$stocks})
observeEvent(input$submit, {updateTabItems(session, inputId = 'menu', selected = 'returns')})
observeEvent(input$next1, {updateTabItems(session, inputId = 'menu', selected = 'risks')})
observeEvent(input$next2, {updateTabItems(session, inputId = 'menu', selected = 'capm')})

# reactive value, selected symbols
val_re <- 
  reactiveValues(stocks_selected = head(symbols_all$Symbol))

# monthly prices
prices <- 
  reactive(to.monthly(prices_daily_all[, val_re$stocks_selected], indexAt = "firstof", OHLC = F))

# monthly returns
returns <-
  reactive(Return.calculate(prices(), method = 'log') %>% na.omit)

# weight of 1/n portfolio
w_eq <- 
  reactive(rep(1/length(val_re$stocks_selected), length(val_re$stocks_selected)))

# monthly return of 1/n portfolio
ret_eq <- 
  reactive(Return.portfolio(returns(), weights = w_eq(), rebalance_on = input$rebal) %>% 
             'colnames<-'('Equal_W'))

# annually return of 1/n portfolio
ret_annu <- 
  reactive(Return.annualized.excess(cbind(returns(), ret_eq()), Rb = 0, scale = 12, geometric = T))

# value box of numbers of stocks selected
output$n_stock <- 
  renderValueBox({valueBox(value = ncol(prices()), 
                           subtitle = "stocks selected",
                           icon = icon('fas fa-chart-line'),
                           color = 'green')})

# value box of annulized return of 1/n portfolio
output$ret_annu <- 
  renderValueBox({valueBox(value = paste(round(ret_annu()[, "Equal_W"], 4) * 100, "%"),
                           subtitle = 'annulized return of 1/N portfolio',
                           icon = icon('fas fa-percentage'),
                           color = 'green')})

# value box of cummulative return of 1/n portfolio
output$ret_value <- 
  renderValueBox({valueBox(value = paste(round((1 + Return.cumulative(ret_eq(), geometric = T))*100, 2), 'Euro'),
                           subtitle = 'value if 100 Euro was invested in the portfolio',
                           icon = icon('fas fa-money-bill-alt'),
                           color = 'green')})

# plot of individuall and 1/n portfolio price trend
output$plot_return <- 
  renderHighchart({
    round(cumsum(cbind(returns(), ret_eq())), 2) %>%
      xts_to_df() %>%
      gather(symbols, returns, -date) %>%
      hchart(type = 'line', hcaes(x = date, y = returns, group = symbols)) %>%
      hc_title(text = 'Cummulative Returns')})