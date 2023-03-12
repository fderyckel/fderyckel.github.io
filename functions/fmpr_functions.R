library(httr)
library(glue)
library(lubridate)
library(readr)

source("api_keys")

get_fmpr_prices <- function(ticker, from = "2001-01-02", to = today()) { 
  base_url <- "https://financialmodelingprep.com/api/v3/"
  endpoint <- 'historical-price-full/'
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', `from` = from, `to` = to, 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json)
  d <- tibble::as_tibble(d$historical)
  write_csv(d, glue("raw_data/", {ticker}, ".csv"))
}

get_fmpr_profile <- function(ticker) { 
  base_url <- "https://financialmodelingprep.com/api/v3/"
  endpoint <- "profile/"
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', `ticker` = ticker, 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble() %>% 
    select(symbol, sector, industry, beta, mktCap) %>% 
    mutate(mktCap = signif(mktCap / 10^9, 4))
  endpoint <- "key-metrics-ttm/"
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  e <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble() %>% 
    select(peRatioTTM, pfcfRatioTTM, priceToSalesRatioTTM, pbRatioTTM, debtToAssetsTTM,
           enterpriseValueOverEBITDATTM) %>% 
    mutate(peRatioTTM = signif(peRatioTTM, 3), pfcfRatioTTM = signif(pfcfRatioTTM, 3), 
           priceToSalesRatioTTM = signif(priceToSalesRatioTTM, 3), pbRatioTTM = signif(pbRatioTTM, 3), 
           debtToAssetsTTM = signif(debtToAssetsTTM, 3), 
           enterpriseValueOverEBITDATTM = signif(enterpriseValueOverEBITDATTM, 3))
  f <- bind_cols(d, e)
  
  return(f)
}

get_fmpr_etf_holder <- function(etf) { 
  base_url <- "https://financialmodelingprep.com/api/v3/"
  endpoint <- 'etf-holder/'
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, etf), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble()
  d <- tibble::add_column(d, etf, .before = 1)
  d <- d %>% select(-isin, -cusip, -sharesNumber) %>% arrange(desc(weightPercentage))
  return(d)
}

get_fmpr_senate_trading <- function(ticker) { 
  base_url <- "https://financialmodelingprep.com/api/v4/"
  endpoint <- 'senate-trading?symbol='
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble()
  d <- tibble::add_column(d, etf, .before = 1)
  d <- d %>% select(-isin, -cusip, -sharesNumber) %>% arrange(desc(weightPercentage))
  return(d)  
}

get_fmpr_senate_trading_rss <- function() { 
  base_url <- "https://financialmodelingprep.com/api/v4/"
  endpoint <- 'senate-trading-rss-feed?page=0'
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble()
  d <- tibble::add_column(d, etf, .before = 1)
  d <- d %>% select(-isin, -cusip, -sharesNumber) %>% arrange(desc(weightPercentage))
  return(d)  
}

get_fmpr_stock_news <- function(ticker){
  base_url <- "https://financialmodelingprep.com/api/v3/"
  endpoint <- 'stock_news?tickers='
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', limit = 50,
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble()
  d <- d |> select(-image, -url)
  return(d)    
}

get_fmpr_upgrade_downgrade <- function(ticker){
  base_url <- "https://financialmodelingprep.com/api/v4/"
  endpoint <- 'upgrades-downgrades?symbol='
  headers = c(`Upgrade-Insecure-Requests` = '1')
  params = list(`datatype` = 'json', 
                `apikey` = fmpr_api_keys)
  res <- httr::GET(url = glue(base_url, endpoint, ticker), 
                   httr::add_headers(.headers = headers), query = params)
  return_json <- httr::content(res, as = "text")
  d <- jsonlite::fromJSON(return_json) %>% tibble::as_tibble() |> 
    mutate(published_date = ymd(str_extract(publishedDate, '^(.*?T)'))) |> rename(news_title = newsTitle)
  d <- d %>% select(-newsURL, newsBaseURL, -publishedDate) 
  return(d)  
}