library('highcharter')
library('tidyverse')
library('PerformanceAnalytics')
library('quantmod')
library('tidyquant')
library('timetk')

par(mfrow = c(1,1))

# ---- Return ----
symbols_all <- 
  read.csv('djia_comp.csv', sep = ',', stringsAsFactors = F) %>% .[-c(2, 22, 27), ]

prices <- 
  getSymbols(symbols_all$Symbol_all,
                     src = 'yahoo',
                     from = '1988-12-31',
                     to = Sys.Date(),
                     auto.assign = T) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-' (symbols_all$Symbol_all)

prices_all <- 
  prices_all['1991-01/']

write.zoo(prices_all, 'djia_prices_all.csv')

# single stock candle chart
highchart(type = "stock") %>%
  hc_add_series(AAPL)

# -------- begin ---------
# get prices dataset
symbols_all <- read.csv('djia_comp.csv', sep = ',', stringsAsFactors = F) %>% .[-c(2, 22, 27), ]
prices_daily_all <- as.xts(read.csv.zoo('djia_prices_all.csv', format = "%Y-%m-%d", sep = ""))

#select symbols
set.seed(123)
symbols <- sample(symbols_all$Symbol, 7)
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




hchart(hist(pf_ret_xts, breaks = 50, plot = F))
chart.CumReturns(cbind(returns, ret_eq), 
                 wealth.index = T, 
                 main = 'Cumulative Return',
                 legend.loc = "topleft")

ret_annu <- Return.annualized.excess(cbind(returns, ret_eq), Rb = 0, scale = 12, geometric = T)

#---- Risk ----
sd <- data.frame(mean = apply(cbind(returns_xts, pf_ret_xts), 2, mean))
sd$sd <- apply(cbind(returns_xts, pf_ret_xts), 2, sd)

sd %>% ggplot(aes(x = sd, y = mean)) + 
  geom_point(size = 2) + 
  ylab("expected return") +
  xlab("standard deviation") + 
  ggtitle("Expected Monthly Returns versus Risk") + 
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) + 
  theme_update(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(x = sd(pf_ret_xts) * 1.11,
                 
                 y =mean(pf_ret_xts),
                 
                 label = "Portfolio"))

tk_tbl(pf_ret_xts, preserve_index = TRUE, rename_index = "date")

chart.TimeSeries(prices_daily, 
                 main = 'Stock Price 1990-2018',
                 legend.loc = 'topleft', 
                 xlab = 'Time', 
                 ylab = "Price in USD",
                 date.format = '%y-%m')


plot.zoo(prices_daily, plot.type = 'single', 
         col = 1:6, main = 'Stock Price 1990-2018',
         xlab = 'Time', ylab = "Price in USD",
         )

highchart(type = 'stock') %>%
  hc_add_series_returns)
