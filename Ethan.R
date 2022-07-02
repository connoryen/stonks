# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "TSA",          # time series stuff
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant",    # pull stock data
              "rvest"         # webscrapping
              )    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
lapply(packages, require, character.only = TRUE)

# ======================================================================
# Scrape industry data from Yahoo Finance
# ======================================================================

# Scrape sector and industry from yahoo finance
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Scrape the sector(s) and industry of a company
#' 
#' This returns the sector(s) and industry of a ticker using data from Yahoo
#' finance.
#' 
#' @param ticker ticker abbreviation of the company in question.
#' 
#' @return vector of sector and industry of the company
#' 
#' @examples
#' scrape_company_type("DoesNotExist")
#' scrape_company_type("GILD")
#' scrape_company_type("AAPL")
scrape_company_type <- function(ticker){
  # Check input
  if(!is.character("ticker")) 
    stop("Error: ticker input is not a string")
  
  rv <- try(
    paste0("https://finance.yahoo.com/quote/", ticker, "/profile") %>%
      read_html() %>%
      # select the node with class "asset-profile-container." This comes from 
      #   inspecting the yahoo finance pages.
      html_nodes(".asset-profile-container") %>%
      # get the text contained within <span> </span?
      html_elements("span") %>%
      html_text2() %>%
      as_tibble() %>%
      slice(c(2,4)) %>%
      # convert to vector
      pull(),
    silent = TRUE
  )
  if (class(rv)=="try-error") NA else {
    c("sector" = rv[1], "industry" = rv[2])
  }
}

# Test ..........
scrape_company_type("DoesNotExist")
scrape_company_type("GILD")
scrape_company_type("AAPL")[["sector"]]

# Run function for all tickers
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# list of tickers
tickers <- stockSymbols()
validTicker <- filter(tickers, Exchange %in% c("AMEX", "NASDAQ", "NYSE"))
validTickers <- tickers[["Symbol"]]


# Start clock
ptm <- proc.time()

# takes ~67s to run 20 rows. (~11 hours to run all 12210 tickers)
data.frame(ticker = validTickers[1:10]) %>%
  rowwise() %>%
  mutate(sector = scrape_company_type(ticker)[["sector"]],
         industry = scrape_company_type(ticker)[["industry"]])

# Stop clock
proc.time() - ptm


# ======================================================================
# Load and Clean House Stock Data 
# ======================================================================

# data from housestockwatcher.com
df0 <- fread('https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.csv')

# EDA for df0
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
table(year(df0$transaction_date))
table(df0$owner)

# clean data frame:
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# list of tickers
tickers <- stockSymbols()
validTickers <- tickers[["Symbol"]]

df <- df0 %>%
  # remove entries w/ unrecognized tickers
  # remove entries without "amount" formatted as "$XXXX - $YYYY"
  # remove "exchange" and "sale_partial" transactions
  # remove transactions prior to 2021
  # remove transactions not owned by "self" or "joint"
  filter(ticker %in% validTickers, 
         str_detect(amount, "\\$[0-9,]+ - \\$[0-9,]+"),
         type %in% c("sale_full", "purchase"),
         year(transaction_date) >= 2021,
         owner %in% c("joint", "self")) %>%
  # change year type to integer
  mutate(disclosure_year = as.integer(disclosure_year)) %>%
  # get lower bound of "amount" -- first dollar amount starting with "$"
  # get upper bound of "amount" -- dollar amount at end of string, 
  #   starting with "$".
  mutate(lower_amount = parse_number(str_match(amount, "\\$[0-9,]+")),
         upper_amount = parse_number(str_match(amount, "\\$[0-9,]+$")))

nrow(df)
# rows deleted:
nrow(df0) - nrow(df)

# test stringr for amounts: 
#zzz <- c("$1,001 - $15,000", "$1,000,001 - $5,000,000", 
#          "$500,001 - $1,000,000", "$1000 - ")
#parse_number(str_match(zzz, "\\$[0-9,]+"))
#parse_number(str_match(zzz, "\\$[0-9,]+$"))
#str_detect(zzz, "\\$[0-9,]+ - \\$[0-9,]+")

# Helper function for price of stocks:
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' Get the price of a stock 
#' 
#' This returns the opening price (other options include "high", "low", and 
#' "close") of a stock ticker at a given date. This function assists with
#' assigning stock prices to congress-reported data where only transaction
#' dates are given. 
#' 
#' @param ticker ticker abbreviation of the stock in question.
#' @param date date of transaction, in "YYYY-MM-DD" format.
#' @param interval the price point for the day used to calculate the stock price.
#' 
#' @return the opening/high/low/closing price of a stock at a given day. Returns
#' NA if an error is return from tidyquant::tq_get. 
#' 
#' @examples 
#' return_price("AAPL", "2020-01-04")
#' return_price("NONEXISTENTTICKER", "2020-01-06")
#' return_price("AAPL", "2020-01-06")
return_price <- function(ticker, date, interval = "open"){
  
  # Check inputs
  if(!is.character("ticker")) 
    stop("Error: ticker input is not a string")
  if(str_detect(date, "[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", 
                negate = TRUE))
    stop("Error: date is not in the proper format")
  if(! interval %in% c("open", "high", "low", "close"))
    stop("Error: interval selection is not valid")
  
  rv <- suppressWarnings(try(tq_get(ticker, 
                                    from = date, 
                                    to = as.Date(date)+1, 
                                    get = "stock.prices")[[interval]],
                             silent = TRUE))
  if("try-error" %in% class(rv)) {
    return(NA)
  } else {
    rv
  }
}

# Test ..........
# NA:
return_price("AAPL", "2020-01-04")
# double: 
return_price("AAPL", "2020-01-06")

# Include price of share at transaction date:
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Start clock
ptm <- proc.time()

# takes ~17s to run 100 rows. (~7 mins to run 2800 rows)
df1 <- df[1:100] %>%
  # Compute price per share (pps) at open. 
  rowwise() %>%
  mutate(pps_open = return_price(ticker, transaction_date))

# Stop clock
proc.time() - ptm


