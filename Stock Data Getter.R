library(tidyquant)
library(quantmod)
library(tidyverse)

useful_tickers <- read_csv("gic_sectors.csv") %>% 
  filter(Exchange %in% c("New York Stock Exchange, Inc.", "Nasdaq"))
tickers <- c(useful_tickers$Ticker, "VTI")

prices <- lapply(tickers, function(ticker) {
  tryCatch({
    getSymbols(ticker, to = as.character(as.Date(today()) + period(1, "day")), auto.assign = FALSE)
  }, error = function(e) { warning(paste("Unable to retrieve data for", ticker)) })
})
valid_data <- !sapply(prices, is.character)
if (any(valid_data)) {
  portfolio_prices_raw <- do.call(merge, lapply(prices[valid_data], function(x) Ad(x)))
  portfolio_prices <- as.data.frame(portfolio_prices_raw) %>%
    rownames_to_column(var = "date") %>% 
    pivot_longer(cols=-date, names_to="Ticker", values_to="Adjusted") %>% 
    mutate(Ticker = str_remove(Ticker, ".Adjusted"),
           date = ymd(date)) %>% 
    arrange(Ticker, date)
}

VTI <- getSymbols("VTI", to = as.character(as.Date(today()) + period(1, "day")), auto.assign = FALSE) %>% 
  Ad() %>% as.data.frame() %>% rownames_to_column(var = "date") %>% 
  pivot_longer(cols=-date, names_to="Ticker", values_to="Adjusted") %>% 
  mutate(Ticker = str_remove(Ticker, ".Adjusted"),
         date = ymd(date)) %>% 
  arrange(Ticker, date)

write_csv(VTI, "VTI.csv")

portfolio_prices <- portfolio_prices %>% 
  left_join(useful_tickers, join_by(Ticker)) %>% 
  mutate(Exchange = ifelse(Exchange == "New York Stock Exchange, Inc.", "NYSE", "NASDAQ"))

write_csv(portfolio_prices, "portfolio_prices_319.csv")

get_fundamentals <- function(tickers, api_key) {
  all_data <- list()
  
  for (ticker in tickers) {
    url <- paste0("https://financialmodelingprep.com/api/v3/key-metrics-ttm/", ticker, "?apikey=", api_key)
    
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text")) %>% as.data.frame()
      if (nrow(data) > 0) {
        data$ticker <- ticker
        all_data[[ticker]] <- data
      }
    } else {
      warning(paste("Failed to fetch data for", ticker))
    }
  }
  
  bind_rows(all_data)
}

api_key <- "dGf8bckYXaBLKU1qunSoVyb4PgSXdLWQ"
tickers <- unique(stock_data$Ticker)

fundamentals <- get_fundamentals(tickers, api_key)

fundamentals_clean <- fundamentals %>%
  select(
    ticker,
    `P/E Ratio` = peRatioTTM,
    `P/B Ratio` = pbRatioTTM,
    `Dividend Yield` = dividendYieldTTM,
    `Revenue per Share` = revenuePerShareTTM,
    `Net Income per Share` = netIncomePerShareTTM,
    `ROE` = roeTTM,
    `Debt-to-Equity Ratio` = debtToEquityTTM
  )

write_csv(fundamentals_clean, "fundamentals_319.csv")

get_income_growth <- function(ticker, api_key) {
  url <- paste0("https://financialmodelingprep.com/api/v3/income-statement/", ticker, "?limit=5&apikey=", api_key)
  data <- fromJSON(content(GET(url), "text")) %>% as.data.frame()
  
  data <- data %>% arrange(desc(date))
  if (nrow(data) >= 2) {
    growth <- (data$revenue[1] - data$revenue[2]) / data$revenue[2] * 100
    return(data.frame(ticker = ticker, `Revenue Growth` = growth))
  } else {
    return(data.frame(ticker = ticker, `Revenue Growth` = NA))
  }
}

get_eps_growth <- function(ticker, api_key) {
  url <- paste0("https://financialmodelingprep.com/api/v3/income-statement/", ticker, "?limit=5&apikey=", api_key)
  data <- fromJSON(content(GET(url), "text")) %>% as.data.frame()
  
  data <- data %>% arrange(desc(date))
  if (nrow(data) >= 2) {
    eps_growth <- (data$eps[1] - data$eps[2]) / data$eps[2] * 100
    return(data.frame(ticker = ticker, `EPS Growth` = eps_growth))
  } else {
    return(data.frame(ticker = ticker, `EPS Growth` = NA))
  }
}

revenue_growth_all <- bind_rows(lapply(tickers, get_income_growth, api_key = api_key))
eps_growth_all <- bind_rows(lapply(tickers, get_eps_growth, api_key = api_key))

library(TTR)  # for technical indicators

calculate_momentum_metrics <- function(df) {
  df <- df %>% arrange(date)
  
  if (nrow(df) < 30 || !"Adjusted" %in% colnames(df)) {
    return(df %>% mutate(ROC = NA, RSI = NA, MACD_val = NA, MACD_Signal = "Any"))
  }
  
  df <- df %>% mutate(Adjusted = as.numeric(Adjusted))
  
  roc_vals <- tryCatch(ROC(df$Adjusted, n = 10) * 100, error = function(e) rep(NA, nrow(df)))
  rsi_vals <- tryCatch(RSI(df$Adjusted, n = 14), error = function(e) rep(NA, nrow(df)))
  macd_vals <- tryCatch(as.data.frame(MACD(df$Adjusted, nFast = 12, nSlow = 26, nSig = 9)), 
                        error = function(e) data.frame(macd = rep(NA, nrow(df)), signal = rep(NA, nrow(df))))
  
  df <- df %>%
    mutate(
      ROC = roc_vals,
      RSI = rsi_vals,
      MACD_val = macd_vals$signal,
      MACD_Signal = case_when(
        is.na(macd_vals$signal) ~ "Any",
        macd_vals$signal > 0 ~ "Buy",
        macd_vals$signal < 0 ~ "Sell",
        TRUE ~ "Any"
      )
    )
  return(df)
}
ticker_list <- unique(stock_data$Ticker)
momentum_data <- bind_rows(lapply(ticker_list, function(t) {
  stock_data %>%
    filter(Ticker == t) %>%
    calculate_momentum_metrics()
}))

latest_momentum <- momentum_data %>%
  group_by(Ticker) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(Ticker, ROC, RSI, MACD_Signal)

metric_data <- stock_data %>% 
  group_by(`Company Name`, Ticker, Exchange, `GICS Sector`, `GICS Industry Group`, `GICS Industry`) %>%
  reframe(`Expected Return` = mean(daily_return),
          `Standard Deviation` = sd(daily_return),
          `Downside Deviation` = sd(pmin(daily_return, 0)))

### Beta
betas <- stock_data %>% 
  group_by(Ticker) %>%
  left_join(benchmark_data %>% select(date, benchmark_daily_return), by = "date") %>%
  tq_performance(Ra = daily_return, Rb = benchmark_daily_return, performance_fun = CAPM.beta, Rf = daily_rf_rate) %>% 
  mutate(`Beta` = CAPM.beta.1) %>% 
  select(-CAPM.beta.1)

### Maximum Drawdown
max_drawdown_1 <- stock_data %>% 
  filter(date >= today() - period("1 year")) %>% 
  tq_performance(Ra = daily_return, performance_fun = maxDrawdown) %>% 
  mutate(`Max Drawdown (1-Year)` = -1*maxDrawdown.1) %>% 
  select(-maxDrawdown.1)

max_drawdown_5 <- stock_data %>% 
  filter(date >= today() - period("5 years")) %>% 
  tq_performance(Ra = daily_return, performance_fun = maxDrawdown) %>% 
  mutate(`Max Drawdown (5-Year)` = -1*maxDrawdown.1) %>% 
  select(-maxDrawdown.1)

### VaR
VaR_daily <- stock_data %>% 
  tq_performance(Ra = daily_return, performance_fun = VaR, Rf = daily_rf_rate, method = "historical") %>% 
  mutate(`VaR Daily` = VaR) %>% 
  select(-VaR)

VaR_annual <- stock_data %>% 
  tq_performance(Ra = yearly_return, performance_fun = VaR, Rf = risk_free_rate, method = "historical") %>% 
  mutate(`VaR Annual` = ifelse(is.na(VaR), 0, VaR)) %>% 
  select(-VaR)

### Join Data Together
metric_data <- metric_data %>% 
  left_join(betas, by = "Ticker") %>% 
  left_join(max_drawdown_1, by = "Ticker") %>%
  left_join(max_drawdown_5, by = "Ticker") %>%
  left_join(VaR_daily, by = "Ticker") %>%
  left_join(VaR_annual, by = "Ticker")

### Ratios
metric_data <- metric_data %>% 
  mutate(`Sharpe Ratio` = ((1+`Expected Return`)^252-1 - risk_free_rate) / (sqrt(252)*`Standard Deviation`),
         `Sortino Ratio` = ((1+`Expected Return`)^252-1 - risk_free_rate) / (sqrt(252)*`Downside Deviation`),
         `Treynor Ratio` = ((1+`Expected Return`)^252-1 - risk_free_rate) / Beta,
         `Calmar Ratio` = ((1+`Expected Return`)^252-1 - risk_free_rate) / (`Max Drawdown (1-Year)` * -1))

# Merge all data together
filter_data <- metric_data %>% 
  left_join(fundamental_data, by = join_by(Ticker == ticker)) %>%
  left_join(latest_momentum, by = "Ticker") %>% 
  relocate(`Company Name`, Ticker, Exchange, `GICS Sector`, `GICS Industry Group`, `GICS Industry`,
           `P/E Ratio`, `P/B Ratio`, `Dividend Yield`, `ROE`, `Debt-to-Equity Ratio`,
           `Revenue per Share`, `Net Income per Share`,
           `ROC`, `RSI`, `MACD_Signal`,
           `Standard Deviation`, Beta, `Max Drawdown (1-Year)`, `Max Drawdown (5-Year)`,
           `VaR Daily`, `VaR Annual`, `Sharpe Ratio`, `Sortino Ratio`, `Treynor Ratio`, `Calmar Ratio`) %>% 
  select(-c(`Expected Return`, `Downside Deviation`))
print(nrow(filter_data))

write_csv(test, "final_data.csv")
