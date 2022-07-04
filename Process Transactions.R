# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant",    # pull stock data
              )    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
lapply(packages, require, character.only = TRUE)

# yfinance
remotes::install_github("ljupch0/yfinance")
library(yfinance)

# ======================================================================
# dplyr wrapper for yfinance
# ======================================================================

#zzz <- get_summaries(c("GILD"))
#names(zzz)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Wrapper for yfinance::get_summaries()
#' 
#' Accommodates 0x0 tibble results when pipping via dplyr
#' 
#' @examples 
#' get_summaries2("GILD")
#' get_summaries2("ARDA-WT")
get_summaries2 <- function(ticker){
  rv <- suppressWarnings(try(get_summaries(ticker)))
  
  if(all(dim(rv) == c(0,0))){
    data.frame(sector = "", industry = "", fte = NA, 
               country = "", state = "", zip = NA)
  } else {
    # merge multiple sectors and industries into a single string
    data.frame(sector = paste(rv$sector, collapse = ","),
               industry = paste(rv$industry, collapse = ","),
               fte = ifelse(is.null(rv$fullTimeEmployees), NA, 
                            rv$fullTimeEmployees),
               country = ifelse(is.null(rv$country), NA, rv$country),
               state = ifelse(is.null(rv$state), NA, rv$state),
               zip = ifelse(is.null(rv$zip), NA, rv$zip))
  }
}

# Test ..........
get_summaries2("GILD")

# ======================================================================
# Load, clean, and process house stock data 
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
#' This returns the of a stock ticker at a given date. This function assists 
#' with assigning stock prices to congress-reported data where only transaction
#' dates are given. 
#' 
#' @param ticker ticker abbreviation of the stock in question.
#' @param date date of transaction, in "YYYY-MM-DD" format.
#' 
#' @return the price of a stock at a given day. Returns
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
                                    get = "stock.prices"),
                             silent = TRUE))
  if("try-error" %in% class(rv) | is.null(dim(rv))) {
    data.frame(pps_open = NA, pps_high = NA, pps_low = NA, pps_close = NA)
  } else {
    data.frame(pps_open = rv$open, pps_high = rv$high, 
               pps_low = rv$low, pps_close = rv$close)
  }
}

# Test ..........
# NA:
return_price("AAPL", "2020-01-04")
# double: 
return_price("AAPL", "2020-01-06")

# Include price of share at transaction date and security summary
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Start clock
ptm <- proc.time()

# Test 1: ~30s/100rows. (~14mins/2800 rows)
# Test 2: 1891s/2455rows.
df1 <- df %>%
  rowwise() %>%
  # Compute price per share (pps)
  mutate(return_price(ticker, transaction_date)) %>%
  # add summary info
  mutate(get_summaries2(ticker)) %>%
  write.csv(., paste0("Processed Data/",
                      Sys.Date(), "_congress_security_histories.csv"), 
            row.names = FALSE)

# Stop clock
proc.time() - ptm
