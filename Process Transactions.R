# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant"    # pull stock data
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

#' Wrapper for yfinance::get_summaries(). Gets sector, industry, #fte, country,
#' state, and zip code for a ticker. 
#' 
#' Accommodates 0x0 tibble results when pipping via dplyr
#' 
#' @examples 
#' get_summaries2("GILD")
#' get_summaries2("ARDA-WT")
get_summaries2 <- function(ticker){
  # Check inputs
  if(class(ticker) != "character") 
    stop("Error: ticker inputs are not all characters")
  if(length(ticker) != 1) 
    stop("Error: number of tickers is invalid. Only 1 ticker is allowed")
  
  rv <- suppressWarnings(try(get_summaries(ticker)))
  
  if(all(dim(rv) == c(0,0))){
    data.frame(ticker = ticker, sector = NA, industry = NA, fte = NA, 
               country = NA, state = NA, zip = NA)
  } else {
    data.frame(ticker = rv$ticker,
               # merge multiple sectors and industries into a single string
               sector = paste(rv$sector, collapse = ","),
               industry = paste(rv$industry, collapse = ","),
               fte = ifelse(is.null(rv$fullTimeEmployees), NA, 
                            rv$fullTimeEmployees),
               country = ifelse(is.null(rv$country), NA, rv$country),
               state = ifelse(is.null(rv$state), NA, rv$state),
               zip = ifelse(is.null(rv$zip), NA, rv$zip))
  }
}

# Test ..........
#df0 <- fread('https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.csv')
#df0[1:100] %>% rowwise() %>% mutate(get_summaries2(ticker)) %>% view()

# ======================================================================
# Helper function for price of stocks
# ======================================================================

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
#' return_price("AAPL", "2020-01-31")
#' return_price(c("AAPL", "MSFT"), "2020-01-31")
#' 
return_price <- function(ticker, date){
  
  # Check inputs
  if(class(ticker) != "character") 
    stop("Error: ticker inputs are not all characters")
  if(str_detect(date, "[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", 
                negate = TRUE))
    stop("Error: date is not in the proper format")
  
  rv <- suppressWarnings(try(tq_get(ticker, 
                                    from = date, 
                                    to = as.Date(date)+1, 
                                    get = "stock.prices"),
                             silent = TRUE))
  if("try-error" %in% class(rv) | is.null(dim(rv))) {
    data.frame(symbol = NA, pps_open = NA, pps_high = NA, pps_low = NA, 
               pps_close = NA)
  } else {
    data.frame(symbol = rv$symbol, pps_open = rv$open, pps_high = rv$high, 
               pps_low = rv$low, pps_close = rv$close)
  }
}

# ======================================================================
# Load, clean, and process house stock data 
# ======================================================================

#' Get a data frame of security transactions made by house members. 
#' 
#' For each transaction, includes the pps (at open, high, low, and close). Also
#' includes summary information from yahoo finance for the purchased security 
#' (sector, industry, number of full time employees (fte), country, state, and 
#' zip code). 
#' 
#' Time tests: (1) 30s/100 rows, (2) 1891s/2455 rows, (3) 233s/648 rows, 
#' (4) 378s/648 rows
#' 
#' @param from transaction starting date (inclusive). Class: Date. 
#' @param to transaction ending date (inclusive). Class: Date. 
#' @param type either "individual" or "full". Specifies type of transactions
#' to pull. For "individual" pulls types "self" and "joint". For full, pulls 
#' all types. Class: character. 
#' @param write write data frame to a csv file.
#' @param path path to save csv.
#' 
get_house_transactions <- function(from = as.Date(paste0(year(Sys.Date()), 
                                                         "-01-01")),
                                   to = Sys.Date(),
                                   type = "individual",
                                   write = TRUE, 
                                   path = "Processed Data/") {
  # Check inputs
  if(!is.Date(from) & is.Date(to)) 
    stop("Error: from and to values are not dates. Try as.Date().")
  if(!type %in% c("individual", "full")) 
    stop("Error: type must either be individual or full")
  if(class(write) != "logical") stop("Error: write must be of class logical")
  
  # data from housestockwatcher.com
  df0 <- fread('https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.csv')
  
  # list of tickers
  validTickers <- stockSymbols()
  validTickers <- validTickers[["Symbol"]]
  
  # clean data frame:
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # test stringr for amounts: 
  #zzz <- c("$1,001 - $15,000", "$1,000,001 - $5,000,000", 
  #          "$500,001 - $1,000,000", "$1000 - ")
  #parse_number(str_match(zzz, "\\$[0-9,]+"))
  #parse_number(str_match(zzz, "\\$[0-9,]+$"))
  #str_detect(zzz, "\\$[0-9,]+ - \\$[0-9,]+")
  
  if(type == "full") {
    df <- df0 %>%
      # remove entries w/ unrecognized tickers
      # remove entries without "amount" formatted as "$XXXX - $YYYY"
      # remove "exchange" transactions
      # remove transactions outside of date range
      filter(ticker %in% validTickers, 
             str_detect(amount, "\\$[0-9,]+ - \\$[0-9,]+"),
             as.Date(transaction_date) >= from, 
             as.Date(transaction_date) <= to, 
             type %in% c("sale_full", "purchase", "sale_partial")) %>%
      # change year type to integer
      mutate(disclosure_year = as.integer(disclosure_year)) %>%
      # get lower bound of "amount" -- first dollar amount starting with "$"
      # get upper bound of "amount" -- dollar amount at end of string, 
      #   starting with "$".
      mutate(lower_amount = parse_number(str_match(amount, "\\$[0-9,]+")),
             upper_amount = parse_number(str_match(amount, "\\$[0-9,]+$")))
  } else {
    df <- df0 %>%
      # remove entries w/ unrecognized tickers
      # remove entries without "amount" formatted as "$XXXX - $YYYY"
      # remove "exchange" transactions
      # remove transactions outside of date range
      # remove transactions not owned by "self" or "joint"
      filter(ticker %in% validTickers, 
             str_detect(amount, "\\$[0-9,]+ - \\$[0-9,]+"),
             as.Date(transaction_date) >= from, 
             as.Date(transaction_date) <= to, 
             type %in% c("sale_full", "purchase", "sale_partial"),
             owner %in% c("joint", "self")) %>%
      # change year type to integer
      mutate(disclosure_year = as.integer(disclosure_year)) %>%
      # get lower bound of "amount" -- first dollar amount starting with "$"
      # get upper bound of "amount" -- dollar amount at end of string, 
      #   starting with "$".
      mutate(lower_amount = parse_number(str_match(amount, "\\$[0-9,]+")),
             upper_amount = parse_number(str_match(amount, "\\$[0-9,]+$")))
  }
  
  # add pps and security summaries
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # operations have to be rowwise for now. 
  rv <- df %>%
    rowwise() %>%
    # Compute price per share (pps)
    mutate(return_price(ticker, transaction_date),
           get_summaries2(ticker)) 
  
  if(write){
    filename <- paste0("transactions_", type, "_from_", as.character(from), 
                       "_to_", as.character(to), ".csv")
    write.csv(rv, paste0(path, filename))
    rv
  } else{
    rv
  }
  
}

# ======================================================================
# run function
# ======================================================================

# Start clock
ptm <- proc.time()

df <- get_house_transactions()

# Stop clock
proc.time() - ptm

# Full data
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Start clock
ptm <- proc.time()

df <- get_house_transactions(type = "full", from = as.Date("2018-01-01"))

# Stop clock
proc.time() - ptm


