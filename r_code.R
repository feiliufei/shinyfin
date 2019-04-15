library('highcharter')
library('tidyverse')
library('PerformanceAnalytics')
library('quantmod')
library('tidyquant')
library('timetk')

par(mfrow = c(1,1))

# ---- Return ----
symbols_all <- 
  read.csv('djia_comp.csv', sep = ';', stringsAsFactors = F) 
write.csv(symbols_all, 'djia_comp.csv', sep = ",")
%>% .[-c(2, 22, 27), ]

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
symbols_all <- read.csv('djia_comp.csv', sep = ';', stringsAsFactors = F)
symbols_all <- symbols_all[-c(2, 22, 27), ]
prices_daily_all <- as.xts(read.csv.zoo('djia_prices_all.csv', format = "%Y-%m-%d", sep = ""))

#select symbols
set.seed(123)
symbols <- sample(symbols_all$Symbol, 6)
prices_daily <- prices_daily_all[, symbols]

# convert to monthly prices and calculate returns
prices <- to.monthly(prices_daily, indexAt = "firstof", OHLC = F)
returns <- Return.calculate(prices, method = "log") %>% 
  na.omit()

prices_long <- prices %>%
  data.frame(date = index(.)) %>%
  gather(asset, prices, -date)

returns_long <- returns %>%
  data.frame(date = index(.)) %>%
  gather(asset, returns, -date)

# monthly returns long format in tibble
# returns_long <- prices %>% 
#   data.frame(date = index(.)) %>%
#   remove_rownames() %>% 
#   gather(asset, prices, -date)%>%
#   group_by(asset) %>% 
#   mutate(returns = log(prices) - log(lag(prices))) %>%
#   select(-prices) %>%
#   na.omit()
hchart(prices_long, 'line', hcaes(x = date, y = prices, group = asset), main = 'Stock Price 1990-2018',
       xlab = 'Time', 
       ylab = "Price in USD")
hchart(returns_long, 'line', hcaes(x = date, y = returns, group = asset))

returns_wide <- returns_long %>%
  spread(asset, returns)

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
  theme_bw()

# plot return together
returns_long %>%
  ggplot(aes(x = date, y = returns)) + 
  geom_line(aes(color = asset)) +
  scale_color_brewer() +
  theme_bw()

# portfolio weight
w <- rep(1/length(symbols), length(symbols))

# calculate portfolio return
pf_ret_xts <- Return.portfolio(returns_xts, w, rebalance_on = "months") %>% 
  `colnames<-`("portfolio")

hchart(pf_ret_xts)
hchart(hist(pf_ret_xts, breaks = 50, plot = F))
chart.CumReturns(cbind(returns_xts, pf_ret_xts), 
                 wealth.index = T, 
                 main = 'Cumulative Return',
                 legend.loc = "topleft")

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
