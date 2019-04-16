shinyServer(function(input, output, session) {
  
  # ---- stock pick ----
  
  # table of company names
  output$id_stock <- 
    renderDataTable(symbols_all, 
                    options = list(pageLength = 10, lengthMenu = c(10, 20, 30)),
                    rownames = F)
  
  observeEvent(input$submit, {val_re$stocks_selected <- input$stocks})
  observeEvent(input$submit, {updateTabItems(session, inputId = 'menu', selected = 'returns')})
  observeEvent(input$next1, {updateTabItems(session, inputId = 'menu', selected = 'risks')})
  observeEvent(input$next2, {updateTabItems(session, inputId = 'menu', selected = 'capm')})
  
  # ---- returns ----
  
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
  
  # monthly return of 1/n portfolio (eq)
  ret_eq <- 
    reactive(Return.portfolio(returns(), weights = w_eq(), rebalance_on = input$rebal) %>% 
               'colnames<-'('Equal_W'))
  
  # annually return of eq
  ret_annu <- 
    reactive(Return.annualized.excess(cbind(returns(), ret_eq()), Rb = 0, scale = 12, geometric = T))
  
  # value box of numbers of stocks selected
  output$n_stock <- 
    renderValueBox({valueBox(value = ncol(prices()), 
                             subtitle = "stocks selected",
                             icon = icon('fas fa-chart-line'),
                             color = 'green')})
  
  # value box of annulized return of eq
  output$ret_annu <- 
    renderValueBox({valueBox(value = paste(round(ret_annu()[, "Equal_W"], 4) * 100, "%"),
                             subtitle = 'annulized return of 1/N portfolio',
                             icon = icon('fas fa-percentage'),
                             color = 'green')})
  
  # value box of cummulative return of eq
  output$ret_value <- 
    renderValueBox({valueBox(value = paste(round((1 + Return.cumulative(ret_eq(), geometric = T))*100, 2), 'Euro'),
                             subtitle = 'value if 100 Euro was invested in the portfolio',
                             icon = icon('fas fa-money-bill-alt'),
                             color = 'green')})
  
  # plot of individuall and eq price trend
  output$plot_return <- 
    renderHighchart({
      round(cumsum(cbind(returns(), ret_eq())), 2) %>%
        xts_to_df() %>%
        gather(symbols, returns, -date) %>%
        hchart(type = 'line', hcaes(x = date, y = returns, group = symbols)) %>%
        hc_title(text = 'Cummulative Returns 1/n v.s individuall stocks')})
  
  
  # ---- risks ----
  
  # table of risk: mean, sd, skewness, kurtosis
  sd <- reactive(data.frame(mean = apply(cbind(returns(), ret_eq()), 2, base::mean),
                            sd = apply(cbind(returns(), ret_eq()), 2, stats::sd),
                            skewness = apply(cbind(returns(), ret_eq()), 2, PerformanceAnalytics::skewness),
                            kurtosis = apply(cbind(returns(), ret_eq()), 2, PerformanceAnalytics::kurtosis)) %>% 
                   round(4) %>%
                   data.frame(symbol = rownames(.)) %>% 
                   select(symbol, mean, sd, skewness, kurtosis))

  # value box of sd eq
  output$sd_eq <- 
    renderValueBox({valueBox(value = round(sd()['Equal_W', 'sd'], 4),
                             subtitle = 'standard deviation of 1/n portfolio',
                             icon = icon('fas fa-search-dollar'),
                             color = 'green')})
  # value box of skewness eq
  output$skewness_eq <-
    renderValueBox({valueBox(value = round(sd()['Equal_W', 'skewness'], 4),
                             subtitle = 'skewness of 1/n portfolio',
                             icon = icon('fas fa-search-dollar'),
                             color = 'green')})

  # value box of kurtosis eq
  output$kurtosis_eq <-
    renderValueBox({valueBox(value = round(sd()['Equal_W', 'kurtosis'], 4),
                             subtitle = 'kurtosis of 1/n portfolio',
                             icon = icon('fas fa-search-dollar'),
                             color = 'green')})
  # output table of risk
  output$sd_table <-
    renderDataTable({DT::datatable(sd(), 
                                   options = list(searching = F, pageLength = 5, lengthMenu = seq(5, 30, 5)),
                                   rownames = F,
                                   colnames = c('ID', 'MEAN', 'SD', 'SKEW.', 'KURTO.'))})
  
  # plot scatterplot of returns v.s risk
  output$ret_risk <-
    renderHighchart({highchart() %>%
                    hc_add_series(sd(), 'scatter', hcaes(x = sd, y = mean, group = symbol)) %>%
                    hc_title(text = 'Expected Monthly Returns versus Risk')})
  
  # plot histogram and density of eq
  output$dis_eq <- 
    renderHighchart({hchart(hist(ret_eq(), breaks = 50, plot = F), name = 'Histogram') %>%
                      hc_add_series(density(ret_eq()), color = 'red', name = "Density") %>%
                      hc_title(text = 'Distribution of 1/n Portfolio Returns')})
  
  # plot rolling sd of eq
  output$roll_sd_eq <- 
    renderHighchart({
      highchart(type = 'stock') %>% 
        hc_add_series(zoo::rollapply(ret_eq(), FUN = stats::sd, width = input$windows), 
                      name = '1/n Portfolio') %>%
        hc_title(text = 'Rolling Volatility') %>%
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_legend(enabled = TRUE)})
  
  # plot rolling skewness of eq
  output$roll_skewness_eq <-
    renderHighchart({
      highchart(type = 'stock') %>% 
        hc_add_series(zoo::rollapply(ret_eq(), FUN = PerformanceAnalytics::skewness, width = input$windows), 
                      name = '1/n Portfolio') %>%
        hc_title(text = 'Rolling Skewness') %>%
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_legend(enabled = TRUE)})
  
  # plot rolling kurtosis of eq
  output$roll_kurtosis_eq <- 
    renderHighchart({
      highchart(type = 'stock') %>% 
        hc_add_series(zoo::rollapply(ret_eq(), FUN = PerformanceAnalytics::kurtosis, width = input$windows), 
                      name = '1/n Portfolio') %>%
        hc_title(text = 'Rolling Kurtosis') %>%
        hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
        hc_legend(enabled = TRUE)})
  
  # ---- portfolio theory ----
  }
  )
