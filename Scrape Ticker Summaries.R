# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant",    # pull stock data
              "rvest"         # web scrapping
)    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
lapply(packages, require, character.only = TRUE)

# yfinance
remotes::install_github("ljupch0/yfinance")
library(yfinance)

# ======================================================================
# Scrape industry data from Yahoo Finance
# ======================================================================

# list of tickers
tickers <- stockSymbols()
validTickers <- tickers[["Symbol"]]

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

# Get Industry and Sector for companies listed in AMEX, NASDAQ, and NYSE
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
         industry = scrape_company_type(ticker)[["industry"]]) %>%
  write.csv(., "Industry_and_Sector.csv", row.names = FALSE)

# Stop clock
proc.time() - ptm

# ======================================================================
# Compare to yfinance package
# ======================================================================

zzz <- get_summaries(c("GILD"))
names(zzz)

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

# Test
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Start clock
ptm <- proc.time()

# test 1: takes ~70s to run first 500 rows. (~28 mins to run all 12210 rows)
# test 2: takes ~136 to run first 1000 rows.
zzz <- data.frame(ticker = validTickers[1:1000]) %>%
  rowwise() %>%
  mutate(get_summaries2(ticker)) %>%
  write.csv(., "security_summaries.csv", row.names = FALSE)
  
# Stop clock
proc.time() - ptm
