library('highcharter')
library('tidyverse')
library('PerformanceAnalytics')
library('quantmod')
library('tidyquant')
library('timetk')

par(mfrow = c(1,1))

# ---- Return ----
symbols_all <- read.csv('/Users/Fei/Google Drive/Projects/R Projects/shinyfin/djia_comp.csv', sep = ',', 
                        stringsAsFactors = F, row.names = NULL) %>% 
  .[-c(2, 22, 27), ] %>%
  arrange(Symbol)

# prices <- 
#   getSymbols(symbols_all$Symbol_all,
#                      src = 'yahoo',
#                      from = '1988-12-31',
#                      to = Sys.Date(),
#                      auto.assign = T) %>%
#   map(~Ad(get(.))) %>%
#   reduce(merge) %>%
#   'colnames<-' (symbols_all$Symbol_all)
# 
# prices_all <- 
#   prices_all['1991-01/']
# 
# write.zoo(prices_all, 'djia_prices_all.csv')

# single stock candle chart
highchart(type = "stock") %>%
  hc_add_series(AAPL)

# -------- begin ---------
# get prices dataset
prices_daily_all <- as.xts(read.csv.zoo('/Users/Fei/Google Drive/Projects/R Projects/shinyfin/djia_prices_all.csv', 
                                        format = "%Y-%m-%d", sep = ""))

#select symbols
symbols <- head(symbols_all$Symbol)
prices_daily <- prices_daily_all[, symbols]

# convert to monthly prices and calculate returns
prices <- to.monthly(prices_daily, indexAt = "firstof", OHLC = F)

returns <- Return.calculate(prices, method = "log") %>% 
  na.omit()

returns_df <- xts_to_df(returns)
returns_wide <- gather(returns_df, symbol, returns, -date)

# prices_long <- prices %>%
#   data.frame(date = index(.)) %>%
#   gather(asset, prices, -date)
# 
# 
# returns_long <- 
#   prices_long %>% 
#   group_by(asset) %>%
#   mutate(returns = log(prices) - log(lag(prices))) %>%
#   select(-prices) %>%
#   na.omit()


hchart(prices_long, 'line', hcaes(x = date, y = prices, group = asset), main = 'Stock Price 1990-2018',
       xlab = 'Time', 
       ylab = "Price in USD")

hchart(returns_long, 'line', hcaes(x = date, y = returns, group = asset))



# plot individual histogram
returns_long %>%
  ggplot(aes(x = returns)) + 
  geom_density(color = 'darkgreen') +
  geom_histogram(alpha = 0.45, binwidth = .005) + 
  facet_wrap(~asset) + 
  guides(fill = F) + 
  ggtitle("Monthly Returns Since 1991") + 
  xlab("monthly returns") + 
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme_bw


# plot return together
returns_long %>%
  ggplot(aes(x = date, y = returns)) + 
  geom_line(aes(color = asset)) +
  scale_color_brewer() +
  theme_bw()

# portfolio weight
w_eq <- rep(1/length(symbols), length(symbols))

# calculate portfolio return
ret_eq <- Return.portfolio(returns, weights = w_eq, rebalance_on = 'years') %>%
  'colnames<-'('Equal_W')

cumsum(cbind(returns, ret_eq)) %>%
  xts_to_df() %>%
  gather(symbols, returns, -date) %>%
  hchart(type = 'line', hcaes(x = date, y = returns, group = symbols))

hchart(xts_to_df(ret_eq), 'scatter', hcaes(x = date, y = Equal_W))

# annually geometric return
ret_annu <- Return.annualized.excess(cbind(returns, ret_eq), Rb = 0, scale = 12, geometric = T)

#---- Risk ----

# plot of Distribution of 1/n Portfolio Returns
hchart(hist(ret_eq, breaks = 50, plot = F), name = 'Histogram') %>%
  hc_add_series(density(ret_eq), color = 'red', name = "Density") %>%
  hc_title(text = 'Distribution of 1/n Portfolio Returns')


sd <- data.frame(mean = apply(cbind(returns, ret_eq), 2, mean),
                 sd = apply(cbind(returns, ret_eq), 2, sd),
                 skewness = apply(cbind(returns, ret_eq), 2, skewness),
                 kurtosis = apply(cbind(returns, ret_eq), 2, kurtosis)) %>%
  round(4)%>%
  data.frame(symbol = rownames(.)) %>% 
  select(symbol, mean, sd, skewness, kurtosis)
  
# plot of Expected Monthly Returns versus Risk
highchart() %>% 
  hc_add_series(sd, 'scatter', hcaes(x = sd, y = mean, group = symbol)) %>%
  hc_title(text = 'Expected Monthly Returns versus Risk')
# 
# sd %>% ggplot(aes(x = sd, y = mean)) + 
#   geom_point(size = 2) + 
#   ylab("expected return") +
#   xlab("standard deviation") + 
#   ggtitle("Expected Monthly Returns versus Risk") + 
#   scale_y_continuous(labels = function(x){ paste0(x, "%")}) + 
#   theme_update(plot.title = element_text(hjust = 0.5))


# plot of colored scatter
xts_to_df(ret_eq) %>%
  mutate(group = case_when(Equal_W <= mean(ret_eq) - sd(ret_eq) ~ '< mean-sd',
                           Equal_W >= mean(ret_eq) + sd(ret_eq) ~ '> mean+sd',
                           TRUE ~ '[mean-sd, mean+sd]')) %>%
  hchart('scatter', hcaes(x = date, y = Equal_W, group = group)) %>%
  hc_title(text = 'Colored Scatter')

# rolling sd
window <- 24
rolling_sd_eq <- rollapply(ret_eq, FUN = sd, width = window) %>% na.omit
highchart(type = 'stock') %>% 
  hc_add_series(rolling_sd_eq, name = '1/n Portfolio') %>%
  hc_title(text = 'Rolling Volatility') %>%
  hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE) %>%
  hc_legend(enabled = TRUE)
